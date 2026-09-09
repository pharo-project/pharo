Extension { #name : 'Form' }

{ #category : '*System-FileRegistry' }
Form class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>

	^((ImageReadWriter allTypicalFileExtensions add: '*'; add: 'form'; yourself)
		includes: suffix)
		ifTrue: [ self services ]
		ifFalse: [#()]
]

{ #category : '*System-FileRegistry' }
Form class >> serviceImageAsBackground [
	"Answer a service for setting the desktop background from a given graphical file's contents"
	<formService>
	^ FileServiceEntry
		provider: self
		label: 'Use graphic as background'
		selector: #setBackgroundFromImageFileNamed:
		description: 'Use the graphic as the background for the desktop'
		buttonLabel: 'Background'
]

{ #category : '*System-FileRegistry' }
Form class >> services [
	"Defines what options are available when a user drags and drop an image file (jpeg, png...) onto the pharo image. These options are defined by methods with the #formService pragma on this class."
	| services |
	services := OrderedCollection new.
	(Pragma allNamed: #formService in: self class) do: [:pragma |
		pragma method selector isUnary ifTrue: [
			services add: (self perform: (pragma method selector))
		]
	].
	^ services asArray
]

{ #category : '*System-FileRegistry' }
Form class >> setBackgroundFromImageFileNamed: aFileName [

	(self fromFileNamed: aFileName) setAsBackground
]
