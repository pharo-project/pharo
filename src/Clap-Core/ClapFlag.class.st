"
I represent a flag (also called option).

A flag is recognized by its form, starting with dashes (e.g. `--foo` in long form or `-f` in short form). Often, flags are optional and take a boolean meaning representing their presence or absence, but they can also be used as named parameters or to accept complex values in the form of positionals.
"
Class {
	#name : #ClapFlag,
	#superclass : #ClapParameterized,
	#category : #'Clap-Core-Specification'
}

{ #category : #evaluating }
ClapFlag class >> basicMeaning [
	^ [ :match |
			match isExplicit and: [ match isMatch ] ]
]

{ #category : #'predefined flags' }
ClapFlag class >> forHelp [
	^ (self withName: 'help')
		description: 'Prints this documentation';
		meaning: [ :match |
			(ClapDocumenter on: (ZnCharacterWriteStream on: match context stdout))
				explain: match parent specification.
			true ]
]

{ #category : #adding }
ClapFlag >> addTo: parentParameter [
	^ parentParameter addFlag: self
]

{ #category : #matching }
ClapFlag >> canMatchWith: word [
	^ word = self shortForm
		or: [ word = self longForm ]
]

{ #category : #accessing }
ClapFlag >> longForm [
	^ '--' , self canonicalName
]

{ #category : #initialization }
ClapFlag >> meaningCollection [
	self meaning: [ :match | match positionalValues ]
]

{ #category : #initialization }
ClapFlag >> meaningScalar [
	"Should only be allowed when there's a single positional"
	self meaning: [ :match | (match at: self positionals first) value ]
]

{ #category : #accessing }
ClapFlag >> shortForm [
	^ '-' , self shortName
]

{ #category : #accessing }
ClapFlag >> shortName [
	^ self canonicalName copyFrom: 1 to: 1
]

{ #category : #documenting }
ClapFlag >> synopsisOn: aStream [
	aStream
		nextPutAll: '--';
		nextPutAll: self canonicalName
]
