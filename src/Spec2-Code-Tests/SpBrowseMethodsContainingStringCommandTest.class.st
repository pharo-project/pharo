Class {
	#name : #SpBrowseMethodsContainingStringCommandTest,
	#superclass : #SpToolCommandTest,
	#category : #'Spec2-Code-Tests'
}

{ #category : #accessing }
SpBrowseMethodsContainingStringCommandTest >> commandClass [

	^ SpBrowseMethodsContainingStringCommand
]

{ #category : #tests }
SpBrowseMethodsContainingStringCommandTest >> testExecute [
	| selectedTextOrLine string |

	command context systemNavigation
		when: #browseMethodsWithSourceString:matchCase:  do: [ :ref :env | string := ref ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
		
	selectedTextOrLine := 'some string'.
	command execute.
	self assert: string equals: 'some string'
]
