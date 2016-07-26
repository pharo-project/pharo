test creation
generateCommentForTestClass: testClass from: aClass

	^ String streamContents: [:stream || name |
			name := testClass name. 
			name first isVowel
				ifTrue: [ stream << 'An ']
				ifFalse:[ stream <<'A '].
			stream << name << ' is a test class for testing the behavior of '<< aClass name ]