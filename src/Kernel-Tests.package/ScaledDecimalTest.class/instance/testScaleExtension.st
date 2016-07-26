tests
testScaleExtension
	"The scale is extended to the larger one in case of arithmetic operation"
	
	#( #* #+ #- #/) do: [:op |
		self assert: (2.5s1 perform: op with: 1.000s3) scale = 3.
		self assert: (3.5000s4 perform: op with: 1.0s1) scale = 4.]