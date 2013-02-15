from twisted.web import xmlrpc, server
import sys

nsuits = 5

def format_card(card):
   # print card
    try:
        card_symbols = '*x23456789T'
        suit_codes = ['1;33','1;34','1','1;32','1;31']
        sys.stdout.write("\033[%sm%c\033[0m" % (suit_codes[card['suit']], card_symbols[card['rank']]))
    except Exception:
        sys.stdout.write('*')

class GTPC(xmlrpc.XMLRPC):
    def xmlrpc_startGame(self):
        return True # assume we want to start

    def xmlrpc_initialize(self, game_id, opponent_id, player_num, hand):
        self.game_id = game_id
        self.opponent_id = opponent_id
        self.player_num = player_num
        self.hand = hand
        print "we are player #", player_num
        return True

    def xmlrpc_getPlay(self, hand, discards, our_expos, their_expos):
        print hand
        print len(hand)
        print discards
        print our_expos
        print their_expos
        max_our_length = max(map(len,our_expos))
        for i in range(nsuits):
            for j in range(max_our_length - len(our_expos[i])):
                sys.stdout.write(' ')
            for c in range(len(our_expos)):
                print c
                format_card(our_expos[c])
            sys.stdout.write(' ')
            if (discards[i] == False):
                sys.stdout.write('-')
            else:
                format_card(discards[i])
            for c in range(len(their_expos)):
                format_card(their_expos[c])
            sys.stdout.write("\n")
        sys.stdout.write("hand: ")
        for c in range(len(hand)):
            format_card(hand[c])
        sys.stdout.write("\n      01234567\n")
        ix = raw_input('card index to play: ')
        pt = raw_input('play to where? (0=expo 1=discard) ')
        df = raw_input('draw from where? (-1=deck, 0+=discard) ')
        return {'card_ix': ix , 'play_to': pt , 'draw_from': df }

    def xmlrpc_drawnCard(self, card_drawn, hand):
        return True

    def xmlrpc_opponentPlay(self, card_played, play_to, draw_from):
        return True

    def xmlrpc_gameEnd(self, score0, score1):
        print "game over! score0=", score0, "score1=", score1
        return True

if __name__ == '__main__':
    print sys.argv
    from twisted.internet import reactor
    r = GTPC()
    reactor.listenTCP(int(sys.argv[1]), server.Site(r))
    reactor.run()
