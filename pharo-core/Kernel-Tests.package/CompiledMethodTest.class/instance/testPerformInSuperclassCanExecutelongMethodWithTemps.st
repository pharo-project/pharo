tests - performing
testPerformInSuperclassCanExecutelongMethodWithTemps
	"self debug: #testPerformInSuperclassCanExecutelongMethodWithTemps"
	"the perform: primitive reuses the context of the method calling it. The primitive adds performed selector arguments to the context variables list. So this means that you can execute some methods but not performed them if the calling methods defined too many temps "
	
	| temp1 temp2 temp3 |
	temp1 := 33.
	temp2 := 666.
	temp3 := 42. 
	self assert: (self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15: withArguments: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) inSuperclass: self class) = 1