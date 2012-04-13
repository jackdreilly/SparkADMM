slice_size = 1000
slice_counter = 0
counter = 0
writefile = open('slice_%i' % slice_counter,'w')
for line in  open('word_counts.admm.data','r'):
    if counter >= slice_size:
	print 'slice',slice_counter
	writefile.close()
	counter = 0
	slice_counter+=1
	writefile = open('slice_%i' % slice_counter,'w')
    writefile.write(line)
    counter+=1
writefile.close()

