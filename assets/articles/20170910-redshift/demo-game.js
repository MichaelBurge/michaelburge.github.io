(function () {
var moves = [
    'start',
    "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1",
    "rnbqkbnr/ppp1pppp/3p4/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1",
    "rnbqkbnr/ppp1pppp/3p4/7Q/8/4P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1",
    "rnbqkb1r/ppp1pppp/3p1n2/7Q/8/4P3/PPPP1PPP/RNB1KBNR w KQkq - 0 1",
    "rnbqkb1r/ppp1pppp/3p1n2/6Q1/8/4P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1",
    "rnbqkb1r/ppp1ppp1/3p1n1p/6Q1/8/4P3/PPPP1PPP/RNB1KBNR w KQkq - 0 1",
    "rnbqkb1r/ppp1ppp1/3p1n1p/8/5Q2/4P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1",
    "rnbqkb1r/ppp1pp2/3p1n1p/6p1/5Q2/4P3/PPPP1PPP/RNB1KBNR w KQkq g6 0 1",
    "rnbqkb1r/ppp1pp2/3p1n1p/6p1/3Q4/4P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1",
    "rnbqkb1r/ppp2p2/3p1n1p/4p1p1/3Q4/4P3/PPPP1PPP/RNB1KBNR w KQkq e6 0 1",
    "rnbqkb1r/ppp2p2/3p1n1p/4p1p1/Q7/4P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1",
    "rn1qkb1r/pppb1p2/3p1n1p/4p1p1/Q7/4P3/PPPP1PPP/RNB1KBNR w KQkq - 0 1",
    "rn1qkb1r/pppb1p2/3p1n1p/4p1p1/2Q5/4P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1",
    "rn1qkb1r/pppb1p2/5n1p/3pp1p1/2Q5/4P3/PPPP1PPP/RNB1KBNR w KQkq - 0 1",
    "rn1qkb1r/pppb1p2/5n1p/3pp1p1/8/1Q2P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1",
    "r2qkb1r/pppb1p2/2n2n1p/3pp1p1/8/1Q2P3/PPPP1PPP/RNB1KBNR w KQkq - 0 1",
    "r2qkb1r/pppb1p2/2n2n1p/3pp1p1/8/1Q2PN2/PPPP1PPP/RNB1KB1R b KQkq - 0 1",
    "r2qkb1r/pppb1p2/2n2n1p/3p2p1/4p3/1Q2PN2/PPPP1PPP/RNB1KB1R w KQkq - 0 1",
    "r2qkb1r/pppb1p2/2n2n1p/3p2p1/4p3/1Q2P3/PPPP1PPP/RNB1KBNR b KQkq - 0 1",
    "r2qkb1r/pppb1p2/5n1p/3p2p1/1n2p3/1Q2P3/PPPP1PPP/RNB1KBNR w KQkq - 0 1",
    "r2qkb1r/pppb1p2/5n1p/3p2p1/1n2p3/1Q2P1P1/PPPP1P1P/RNB1KBNR b KQkq - 0 1",
    "r3kb1r/pppbqp2/5n1p/3p2p1/1n2p3/1Q2P1P1/PPPP1P1P/RNB1KBNR w KQkq - 0 1",
    "r3kb1r/pppbqp2/5n1p/3p2p1/1n2p3/1Q2P1P1/PPPPBP1P/RNB1K1NR b KQkq - 0 1",
    "r3kb1r/pppb1p2/5n1p/2qp2p1/1n2p3/1Q2P1P1/PPPPBP1P/RNB1K1NR w KQkq - 0 1",
    "r3kb1r/pppb1p2/5n1p/2qp2p1/1n2p3/1QP1P1P1/PP1PBP1P/RNB1K1NR b KQkq - 0 1",
    "r3kb1r/pppb1p2/2n2n1p/2qp2p1/4p3/1QP1P1P1/PP1PBP1P/RNB1K1NR w KQkq - 0 1",
    "r3kb1r/pQpb1p2/2n2n1p/2qp2p1/4p3/2P1P1P1/PP1PBP1P/RNB1K1NR b KQkq - 0 1",
    "1r2kb1r/pQpb1p2/2n2n1p/2qp2p1/4p3/2P1P1P1/PP1PBP1P/RNB1K1NR w KQk - 0 1",
    "1r2kb1r/p1pb1p2/Q1n2n1p/2qp2p1/4p3/2P1P1P1/PP1PBP1P/RNB1K1NR b KQk - 0 1",
    "1rb1kb1r/p1p2p2/Q1n2n1p/2qp2p1/4p3/2P1P1P1/PP1PBP1P/RNB1K1NR w KQk - 0 1",
    "1rb1kb1r/p1p2p2/2n2n1p/2qp2p1/Q3p3/2P1P1P1/PP1PBP1P/RNB1K1NR b KQk - 0 1",
    "2b1kb1r/p1p2p2/2n2n1p/2qp2p1/Qr2p3/2P1P1P1/PP1PBP1P/RNB1K1NR w KQk - 0 1",
    "2b1kb1r/p1p2p2/2n2n1p/2qp2p1/1r2p3/2P1P1P1/PPQPBP1P/RNB1K1NR b KQk - 0 1",
    "2b1kb1r/p1p2p2/5n1p/2qp2p1/1r1np3/2P1P1P1/PPQPBP1P/RNB1K1NR w KQk - 0 1",
    "2b1kb1r/p1p2p2/5n1p/2qp2p1/1r1Pp3/2P3P1/PPQPBP1P/RNB1K1NR b KQk - 0 1",
    "2b1kb1r/p1p2p2/5n1p/2qp2p1/3rp3/2P3P1/PPQPBP1P/RNB1K1NR w KQk - 0 1",
    "2b1kb1r/p1p2p2/5n1p/2qp2p1/3rp3/2P3P1/PPQPBP1P/RNB2KNR b k - 0 1",
    "4kb1r/p1p2p2/5n1p/2qp2p1/3rp1b1/2P3P1/PPQPBP1P/RNB2KNR w k - 0 1",
    "4kb1r/p1p2p2/B4n1p/2qp2p1/3rp1b1/2P3P1/PPQP1P1P/RNB2KNR b k - 0 1",
    "4kb1r/p1p2p2/B1q2n1p/3p2p1/3rp1b1/2P3P1/PPQP1P1P/RNB2KNR w k - 0 1",
    "4kb1r/p1p2p2/B1q2n1p/3p2p1/3rp1b1/2P3P1/PPQP1P1P/RNB1K1NR b k - 0 1",
    "4kb1r/p1p2p2/B1q2n1p/3p2p1/r3p1b1/2P3P1/PPQP1P1P/RNB1K1NR w k - 0 1",
    "4kb1r/p1p2p2/2q2n1p/3p2p1/r3p1b1/2P3P1/PPQP1P1P/RNB1KBNR b k - 0 1",
    "4kb1r/p1p2p2/2q2n1p/6p1/r2pp1b1/2P3P1/PPQP1P1P/RNB1KBNR w k - 0 1",
    "4kb1r/p1p2p2/2q2n1p/6p1/r2pp1b1/1QP3P1/PP1P1P1P/RNB1KBNR b k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb1pp1b1/1QP3P1/PP1P1P1P/RNB1KBNR w k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb1pp1bP/1QP3P1/PP1P1P2/RNB1KBNR b k h3 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb1p2bP/1QP1p1P1/PP1P1P2/RNB1KBNR w k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb1p2bP/1QP1pPP1/PP1P4/RNB1KBNR b k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb1p2bP/1QP2PP1/PP1p4/RNB1KBNR w k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb1p2bP/1QP2PP1/PP1K4/RNB2BNR b k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb4bP/1Qp2PP1/PP1K4/RNB2BNR w k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb4bP/1Qp2PP1/PPK5/RNB2BNR b k - 0 1",
    "4k2r/p1p2p2/2q2n1p/5bp1/rb5P/1Qp2PP1/PPK5/RNB2BNR w k - 0 1",
    "4k2r/p1p2p2/2q2n1p/5bp1/rb5P/1QpB1PP1/PPK5/RNB3NR b k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb5P/1Qpb1PP1/PPK5/RNB3NR w k - 0 1",
    "4k2r/p1p2p2/2q2n1p/6p1/rb5P/1QpK1PP1/PP6/RNB3NR b k - 0 1",
    "4k2r/p1p2p2/5n1p/3q2p1/rb5P/1QpK1PP1/PP6/RNB3NR w k - 0 1",
    "4k2r/p1p2p2/5n1p/3Q2p1/rb5P/2pK1PP1/PP6/RNB3NR b k - 0 1",
    "4k2r/p1p2p2/7p/3n2p1/rb5P/2pK1PP1/PP6/RNB3NR w k - 0 1",
    "4k2r/p1p2p2/7p/3n2p1/rb5P/2pK1PP1/PP5R/RNB3N1 b k - 0 1",
    "5rk1/p1p2p2/7p/3n2p1/rb5P/2pK1PP1/PP5R/RNB3N1 w - - 0 1",
    "5rk1/p1p2p2/7p/3n2p1/rb5P/2pK1PPN/PP5R/RNB5 b - - 0 1",
    "4r1k1/p1p2p2/7p/3n2p1/rb5P/2pK1PPN/PP5R/RNB5 w - - 0 1",
    "4r1k1/p1p2p2/7p/3n2p1/rb3P1P/2pK2PN/PP5R/RNB5 b - - 0 1",
    "6k1/p1p2p2/7p/3n2p1/rb3P1P/2pKr1PN/PP5R/RNB5 w - - 0 1",
    "6k1/p1p2p2/7p/3n2p1/rbK2P1P/2p1r1PN/PP5R/RNB5 b - - 0 1",
    "6k1/p1p2p2/7p/3n2p1/rbK2P1P/4r1PN/Pp5R/RNB5 w - - 0 1",
    "6k1/p1p2p2/7p/3n2p1/rbK2P1P/4r1PN/PB5R/RN6 b - - 0 1",
    "6k1/p1p2p2/7p/3n2p1/r1K2P1P/2b1r1PN/PB5R/RN6 w - - 0 1",
    "6k1/p1p2p2/7p/2Kn2p1/r4P1P/2b1r1PN/PB5R/RN6 b - - 0 1",
    "6k1/p1p2p2/7p/r1Kn2p1/5P1P/2b1r1PN/PB5R/RN6 w - - 0 1",
    "6k1/p1p2p2/2K4p/r2n2p1/5P1P/2b1r1PN/PB5R/RN6 b - - 0 1",
    "6k1/p1p2p2/2K1r2p/r2n2p1/5P1P/2b3PN/PB5R/RN6 w - - 0 1",
    "6k1/pKp2p2/4r2p/r2n2p1/5P1P/2b3PN/PB5R/RN6 b - - 0 1",
    "6k1/pKp2p2/4r2p/1r1n2p1/5P1P/2b3PN/PB5R/RN6 w - - 0 1",
    "2K3k1/p1p2p2/4r2p/1r1n2p1/5P1P/2b3PN/PB5R/RN6 b - - 0 1",
    "2K3k1/p1p2p2/4r2p/3n2p1/5P1P/2b3PN/Pr5R/RN6 w - - 0 1",
    "2K3k1/p1p2p2/4r2p/3n2p1/5P1P/2b3PN/Pr6/RN5R b - - 0 1",
    "2K3k1/p1p2p2/4r2p/b2n2p1/5P1P/6PN/Pr6/RN5R w - - 0 1",
    "2K3k1/p1p2p2/4r2p/b2n1Pp1/7P/6PN/Pr6/RN5R b - - 0 1",
    "2K3k1/p1p2p2/3r3p/b2n1Pp1/7P/6PN/Pr6/RN5R w - - 0 1",
    "2K3k1/p1p2p2/3r3p/b2n1Pp1/7P/N5PN/Pr6/R6R b - - 0 1",
    "2K3k1/p1p1np2/3r3p/b4Pp1/7P/N5PN/Pr6/R6R w - - 0 1"
];

    var move = 0;
    var cfg1 = {
        pieceTheme: wikipedia_piece_theme,
        //pieceTheme: '/assets/articles/20170910-redshift/{piece}.png',
        position: 'start',
        showErrors: 'console'
    };
    var cfg2 = {
        pieceTheme: wikipedia_piece_theme,
        position: '8/8/8/5K2/4B3/1k1b4/8/8 w - - 0 1',
        showErrors: 'console'
    };
    
    var board1 = window.ChessBoard('demo-game', cfg1);
    var board2 = window.ChessBoard('illegal', cfg2);
    
    document.getElementById("previousBtn").onclick = function() {
        move--;
        if (move < 0)
            move = 0;
        board1.position(moves[move]);
    };

    document.getElementById("nextBtn").onclick = function() {
        move++;
        if (move >= moves.length)
            move = moves.length - 1;
        board1.position(moves[move]);
    };
})();
