Class {
	#name : 'ReExtractTempDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReExtractTempDriverTest >> testInvalidNameAndAbandon [

	| rbclasses driver request |
	rbclasses := RBClassEnvironment classes: { MyClassA }.
	driver := ReExtractTempDriver new
		          scopes: { rbclasses };
		          extract: (23 to: 24)
					from: #methodForPushDown
		          in: MyClassA.
	self setUpDriver: driver.

	request := MockObject new.
	request on: #title: respond: 2.
	request on: #label: respond: 2.
	request on: #text: respond: 2.
	request on: #acceptLabel: respond: 2.
	request on: #cancelLabel: respond: 2.
	request on: #onAccept: respond: 3. 
	request on: #openModal respond: nil. 
	driver requestDialog: request.
	
	driver runRefactoring.
	"Renaming the references to the variable is not encapsulated in change objects, therefore the only change is the renaming of the variable itself."


	self assert: driver refactoring changes changes size equals: 0
]

{ #category : 'tests' }
ReExtractTempDriverTest >> testValidNameSuccessfulRename [

	| rbclasses driver request |
	rbclasses := RBClassEnvironment classes: { MyClassA }.
	driver := ReExtractTempDriver new
		          scopes: { rbclasses };
		          extract: (23 to: 24)
					from: #methodForPushDown
		          in: MyClassA.
	self setUpDriver: driver.

	request := MockObject new.
	request on: #title: respond: 2.
	request on: #label: respond: 2.
	request on: #text: respond: 2.
	request on: #acceptLabel: respond: 2.
	request on: #cancelLabel: respond: 2.
	request on: #onAccept: respond: 'fooBar'. 
	request on: #openModal respond: 'fooBar'. 
	driver requestDialog: request.
	
	driver runRefactoring.
	"Renaming the references to the variable is not encapsulated in change objects, therefore the only change is the renaming of the variable itself."


	self assert: driver refactoring changes changes size equals: 1
]
