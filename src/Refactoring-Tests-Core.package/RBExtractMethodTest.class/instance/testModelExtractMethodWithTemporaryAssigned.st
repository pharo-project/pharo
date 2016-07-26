tests
testModelExtractMethodWithTemporaryAssigned
	| class refactoring |
	model := RBNamespace new.
	class := model classNamed: self class name.
	class 
		compile: 'foo 				| temp bar | 				bar := 5. 				temp := bar * bar. 				Transcript show: temp printString; cr. 				^temp * temp'
		classified: #(#accessing).
	refactoring := RBExtractMethodRefactoring 
				model: model
				extract: (26 to: 102)
				from: #foo
				in: class.
	self setupMethodNameFor: refactoring toReturn: #foobar.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo) 
				= (RBParser parseMethod: 'foo | temp | temp := self foobar. ^temp * temp').
	self 
		assert: ((class parseTreeFor: #foobar) = (RBParser 
						parseMethod: 'foobar | bar temp | bar := 5. temp := bar * bar. Transcript show: temp printString; cr. ^temp.')) |
				((class parseTreeFor: #foobar) = (RBParser 
						parseMethod: 'foobar | temp bar | bar := 5. temp := bar * bar. Transcript show: temp printString; cr. ^temp.'))