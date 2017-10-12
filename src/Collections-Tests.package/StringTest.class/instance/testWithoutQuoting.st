tests - internet
testWithoutQuoting
 	#(
               '"foo"' 'foo'
               '''foo''' 'foo'
               '"foo''' '"foo'''
               '''foo"' '''foo"'
               '"foo' '"foo'
               'foo"' 'foo"'
               'foo' 'foo') 
	pairsDo: [ :before :after |
		self assert: before withoutQuoting = after ]