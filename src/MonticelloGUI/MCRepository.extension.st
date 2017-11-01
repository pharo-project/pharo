Extension { #name : #MCRepository }

{ #category : #'*MonticelloGUI' }
MCRepository class >> fillInTheBlankConfigure [
	^ self fillInTheBlankConfigure: self creationTemplate
			
]

{ #category : #'*MonticelloGUI' }
MCRepository class >> fillInTheBlankConfigure: aTemplateString [ 
	| chunk repo |
	aTemplateString
		ifNil: [^ false].
	chunk := UIManager default
				multiLineRequest: self fillInTheBlankRequest
				initialAnswer: aTemplateString
				answerHeight: 150.
	(chunk notNil and: [ chunk notEmpty ])
		ifTrue: [repo := self readFrom: chunk readStream.
			repo creationTemplate: chunk].
	^ repo
]

{ #category : #'*MonticelloGUI' }
MCRepository >> morphicOpen [
	self morphicOpen: nil
]

{ #category : #'*MonticelloGUI' }
MCRepository >> morphicOpen: aWorkingCopy [
	self subclassResponsibility 
]

{ #category : #'*MonticelloGUI' }
MCRepository >> openAndEditTemplateCopy [
	^ self class fillInTheBlankConfigure: (self asCreationTemplate ifNil: [^nil])
]
