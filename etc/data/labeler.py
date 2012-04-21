fn = 'rcv1_topics_train.svm'
with open('labeled_rcv1.admm.data','w') as fnw:
    for ind, line in enumerate(open(fn,'r')):
	print 'next line'
	fnw.write('%i %s' % (ind, line))
	
