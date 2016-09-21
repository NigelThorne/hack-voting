        /*global Elm, firebase, firebaseConfig */

(function () {
    // Start the Elm App.
    var app = Elm.App.fullscreen();

    // Initialize Firebase
    var firebaseApp = firebase.initializeApp(firebaseConfig);

    // Firebase Auth.
    app.ports.authenticate.subscribe(function () {
        firebase.auth().signInAnonymously()
            .catch(app.ports.authError.send);
    });

    // Looks like this doesn't work at all if it fires before the Elm
    // app has finished initialising. But why hasn't it, by this
    // stage???

        firebaseApp.auth()
            .onAuthStateChanged(app.ports.authStateChanged.send);

    // setTimeout(function() {
    // }, 1);

    var database = firebase.database();

    var deckPath = database.ref('/deck');
    var roomPath = database.ref('/rooms/'+ window.location.pathname);

    app.ports.roomListen.subscribe(function () {
        console.log('LISTENING', roomPath.toString());
        roomPath.on(
            'value',
            function(snapshot) {
                var rawValue = snapshot.val();
                console.log('HEARD', rawValue);

                app.ports.room.send(JSON.stringify(rawValue));
            },
            app.ports.roomError.send
        );
        console.log('LISTENING DECK', deckPath.toString());
        deckPath.on(
            'value',
            function(snapshot) {
                var rawValue = snapshot.val();
                console.log('HEARD DECK', rawValue);
                app.ports.deck.send(JSON.stringify(rawValue));
            },
            app.ports.deckError.send
        );
  });

    app.ports.roomSilence.subscribe(function () {
        console.log('SILENCING', roomPath.toString());
        roomPath.off('value');
        deckPath.off('value');
    });

    // Show Votes.
    var showVotesPath = roomPath.child('showVotes');

    app.ports.votingCompleteSend.subscribe(function (show) {
        showVotesPath.set(show)
            .catch(app.ports.votingCompleteSendError.send);
    });

    // Voting.
    var votePath = roomPath.child('votes');

    app.ports.voteSend.subscribe(function (msg) {
        var uid = msg[0],
            vote = msg[1],
            path = votePath.child(uid);

        showVotesPath.set(false)
            .catch(app.ports.votingCompleteSendError.send);
        path.set(vote)
            .catch(app.ports.voteSendError.send);
    });

    app.ports.topicSend.subscribe(function (msg) {
        var topic = msg,
            path = roomPath.child('topic');

        votePath.set(null)
            .catch(app.ports.voteSendError.send);
        path.set(topic)
            .catch(app.ports.voteSendError.send);
    });

    var namePath = roomPath.child('voters');

    app.ports.nameSend.subscribe(function (msg) {
        var uid = msg[0],
            name = msg[1],
            path = namePath.child(uid);

        path.set(name)
            .catch(app.ports.nameSendError.send);
    });
}());
