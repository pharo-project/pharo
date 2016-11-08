methods
testMethod1
	^self testMethod1 
		, ([:each | each testMethod1] value: #(#(#testMethod1) 2 #testMethod1))