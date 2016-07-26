tests
testSorted
	| presentation | 
	presentation := GLMListPresentation new. 
	presentation entity: #(#foo #bar #blurp).
	self assert: presentation displayValue equals: #(#foo #bar #blurp).
	presentation sorted. 
	self assert: presentation displayValue equals: #(#bar #blurp #foo).
	presentation sorted: [:a :b | a size < b size].  
	self assert: presentation displayValue equals: #(#bar #foo #blurp).