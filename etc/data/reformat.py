from pylab import *
from collections import defaultdict
def parse_line(line):
    line = line[:-1]
    topics, rest = line.split('  ')
    topics = map(int,topics.split(','))
    words = [(int(word), float(freq)) for word,freq in [pair.split(':') for pair in rest.split(' ')]]
    return (topics,words)
with open('rcv1_topics_train.svm','r') as f_read:
    docs = []
    counter = 0
    for line in f_read:
	docs.append(parse_line(line))


def main1():
    with open('word_counts.admm.data','w') as f_words:
        with open('topic_hits.admm.data','w') as f_topics:
            for topics,words in docs:
                f_words.write(':'.join(map(str,words)))
                f_words.write('\n')
                f_topics.write(' '.join(str(int(i in topics)) for i in range(103)))
                f_topics.write('\n')

def main2():
    with open('topic_hits_transpose.admm.data','w') as f_topics:
        for i in range(103):
            f_topics.write(' '.join(str(int(i in topics)) for topics,words in docs))
            f_topics.write('\n')
main2()            
