printing
printOn: stream
	stream 
		<< (self remoteName ifNil: [ 'noname' ]) 
		<< ' (' << self url << ')'