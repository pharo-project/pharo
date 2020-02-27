"
STON implements serialization and materialization using the Smalltalk Object Notation format.

I am a class side facade offering a high level API to write and read objects using STON.

U s a g e

Basic operations

  #toString:
  #fromString:

  STON toString: DisplayScreen boundingBox.
  STON fromString:  'Rectangle{#origin:Point[0,0],#corner:Point[1920,1030]}'.

  STON toString: { DateAndTime now. Float pi. 1 to: 10 by: 2. 3 days }.
  STON fromString:  '[DateAndTime[''2016-03-15T13:57:59.462422+01:00''],3.141592653589793,Interval{#start:1,#stop:10,#step:2},Duration{#nanos:0,#seconds:259200}]'

You can also read from or write to streams

  #fromStream:
  #put:onStream:

There is also the option to do pretty printing (indenting, multi line output) 

  #toStringPretty:
  #put:onStreamPretty:

STON is more or less a superset of JSON and is backwards compatible with JSON while parsing, and can be compatible with it while writing. The important differences (and the whole reason why STON exists in the first place) are 

  - class information (except for lists (Array) and maps (Dictionary))
  - proper handling of shared and circular references
  - more Smalltalk like syntax (Symbols with #, single qouted Strings, nil instead of null)
  - more defined special types (Date, Time, DataAndTime, ByteArray, Point)

Parsing JSON is done using #fromString: or #fromStream: with the results being composed of Arrays and Dictionaries.

Writing objects as JSON is done using: 

  #toJsonString[Pretty]:
  #put:asJsonOnStream[Pretty]:

Note that you can only write Arrays and Dictionaries !

For a much more sophisticated JSON parser/writer implementation, have a look at NeoJSON.

Like JSON, STON does not allow for comments. However, a preprocessor option can skip C style comments before parsing.

I also define some contants used in the implementation: the class used as list, map and association, as well as the optional class name key (used when reading objects using an unknown class).


I m p l e m e n t a t i o n

The 2 key methods are

  #stonOn:
  #fromSton:

which work together with STONWriter and STONReader; read their class comments for all configuration options (you can use the #reader and #writer methods to avoid referring to these classes directly).

Several methods are used to support and/or control the implementation

  #stonName - defines the external name for a class
  #stonAllInstVarNames - defines which instance variable to write
  #stonContainSubObjects - shortcut looking into objects for subobjects
  #stonShouldWriteNilInstVars - option to skip writing nil valued instance variables


S y n t a x

	value
	  primitive-value
	  object-value
	  reference
	  nil
	primitive-value
	  number
	  true
	  false
	  symbol
	  string
	object-value
	  object
	  map
	  list
	object
	  classname map
	  classname list
	reference
	  @ int-index-previous-object-value
	map
	  {}
	  { members }
	members
	  pair
	  pair , members
	pair
	  string : value
	  symbol : value
	  number : value
	list
	  []
	  [ elements ]
	elements
	  value 
	  value , elements
	string
	  ''
	  ' chars '
	chars
	  char
	  char chars
	char
	  any-printable-ASCII-character-
	    except-'-""-or-\
	  \'
	  \""
	  \\
	  \/
	  \b
	  \f
	  \n
	  \r
	  \t
	  \u four-hex-digits
	symbol
	  # chars-limited
	  # ' chars '
	chars-limited
	  char-limited
	  char-limited chars-limited
	char-limited
	  a-z A-Z 0-9 - _ . /
	classname
	  uppercase-alpha-char alphanumeric-char
	number
	  int
	  int denominator
	  int denominator scale
	  int frac
	  int exp
	  int frac exp
	int
	  digit
	  digit1-9 digits 
	  - digit
	  - digit1-9 digits
	denominator
	  / digits
	scale
	  s digits
	frac
	  . digits
	exp
	  e digits
	digits
	  digit
	  digit digits
	e
	  e
	  e+
	  e-
	  E
	  E+
	  E-

"
Class {
	#name : #STON,
	#superclass : #Object,
	#category : #'STON-Core-Facade'
}

{ #category : #accessing }
STON class >> associationClass [
	^ Association
]

{ #category : #accessing }
STON class >> classNameKey [
	^ #className
]

{ #category : #convencience }
STON class >> fromStream: readStream [
	^ (self reader on: readStream) next
]

{ #category : #convencience }
STON class >> fromStreamWithComments: readStream [
	^ (self reader on: (STONCStyleCommentsSkipStream on: readStream)) next
]

{ #category : #convencience }
STON class >> fromString: string [
	^ self fromStream: string readStream
]

{ #category : #convencience }
STON class >> fromStringWithComments: string [
	^ self fromStreamWithComments: string readStream
]

{ #category : #accessing }
STON class >> jsonWriter [
	^ STONWriter new
			jsonMode: true;
			referencePolicy: #error;
			yourself
]

{ #category : #accessing }
STON class >> listClass [
	^ Array
]

{ #category : #accessing }
STON class >> mapClass [
	^ Dictionary
]

{ #category : #convencience }
STON class >> put: object asJsonOnStream: stream [
	(self jsonWriter on: stream) nextPut: object
]

{ #category : #convencience }
STON class >> put: object asJsonOnStreamPretty: stream [
	(self jsonWriter on: stream)
		prettyPrint: true; 
		nextPut: object
]

{ #category : #convencience }
STON class >> put: object onStream: stream [
	(self writer on: stream) nextPut: object
]

{ #category : #convencience }
STON class >> put: object onStreamPretty: stream [
	(self writer on: stream)
		prettyPrint: true; 
		nextPut: object
]

{ #category : #accessing }
STON class >> reader [
	^ STONReader new
]

{ #category : #convencience }
STON class >> toJsonString: object [
	^ String streamContents: [ :stream |
		self put: object asJsonOnStream: stream ]
]

{ #category : #convencience }
STON class >> toJsonStringPretty: object [
	^ String streamContents: [ :stream |
		self put: object asJsonOnStreamPretty: stream ]
]

{ #category : #convencience }
STON class >> toString: object [
	^ String streamContents: [ :stream |
		self put: object onStream: stream ]
]

{ #category : #convencience }
STON class >> toStringPretty: object [
	^ String streamContents: [ :stream |
		self put: object onStreamPretty: stream ]
]

{ #category : #accessing }
STON class >> writer [
	^ STONWriter new
]
