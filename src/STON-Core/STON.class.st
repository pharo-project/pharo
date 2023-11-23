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
  - more Smalltalk like syntax (Symbols with #, single quoted Strings, nil instead of null)
  - more defined special types (Date, Time, DataAndTime, ByteArray, Point)

Parsing JSON is done using #fromString: or #fromStream: with the results being composed of Arrays and Dictionaries.

Writing objects as JSON is done using: 

  #toJsonString[Pretty]:
  #put:asJsonOnStream[Pretty]:

Note that you can only write Arrays and Dictionaries !

For a much more sophisticated JSON parser/writer implementation, have a look at NeoJSON.

Like JSON, STON does not allow for comments. However, a preprocessor option can skip C style comments before parsing.

I also define some constants used in the implementation: the class used as list, map and association, as well as the optional class name key (used when reading objects using an unknown class).


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
	#name : 'STON',
	#superclass : 'Object',
	#category : 'STON-Core-Facade',
	#package : 'STON-Core',
	#tag : 'Facade'
}

{ #category : 'accessing' }
STON class >> associationClass [
	"Return the class of STON associations, Association, a system wide constant"

	"(STON fromString: '#foo : 100') class >>> STON associationClass"

	^ Association
]

{ #category : 'accessing' }
STON class >> classNameKey [
	"Return the key that will be used to add a property with the class name
	when an unknown class is read and the option STONReader>>#acceptUnknownClasses: is true,
	by default #className. Such unknown classes are returned as generic maps."

	"((STON reader acceptUnknownClasses: true; on: 'FooBarBaz { #foo : 100 }' readStream; next) at: STON classNameKey) >>> #FooBarBaz"

	^ #className
]

{ #category : 'convencience' }
STON class >> fromStream: readStream [
	"Parse and materialize the STON representation in the character readStream"

	"(STON fromStream: 'Point[1,2]' readStream) >>> (1@2)"

	"FileLocator temp / ('{1}.ston' format: { Time millisecondClockValue }) in: [ :file |
		file writeStreamDo: [ :out | STON put: 1@2 onStream: out ].
		file readStreamDo: [ :in | STON fromStream: in ] ]"

	^ (self reader on: readStream) next
]

{ #category : 'convencience' }
STON class >> fromStreamWithComments: readStream [
	"Parse and materialize the STON representation in the character readStream,
	skipping C-style comments"

	"(STON fromStreamWithComments: 'Point[1,/*comment*/2]' readStream) >>> (1@2)"

	^ (self reader on: (STONCStyleCommentsSkipStream on: readStream)) next
]

{ #category : 'convencience' }
STON class >> fromString: string [
	"Parse and materialize the STON representation in string"

	"(STON fromString: 'Point[1,2]') >>> (1@2)"

	^ self fromStream: string readStream
]

{ #category : 'convencience' }
STON class >> fromStringWithComments: string [
	"Parse and materialize the STON representation in string,
	skipping C-style comments"

	"(STON fromStringWithComments: 'Point[1,/*comment*/2]') >>> (1@2)"

	^ self fromStreamWithComments: string readStream
]

{ #category : 'accessing' }
STON class >> jsonWriter [
	"Shortcut to create a new STONWriter instance configured to output JSON"

	^ STONWriter new
			jsonMode: true;
			referencePolicy: #error;
			yourself
]

{ #category : 'accessing' }
STON class >> listClass [
	"Return the class of STON lists, Array, a system wide constant"

	"(STON fromString: '[1 , 2 , 3]') class >>> STON listClass"

	^ Array
]

{ #category : 'accessing' }
STON class >> mapClass [
	"Return the class of STON maps, Dictionary, a system wide constant"

	"(STON fromString: '{ #foo : 1 , #bar : 2 }') class >>> STON mapClass"

	^ Dictionary
]

{ #category : 'convencience' }
STON class >> put: object asJsonOnStream: writeStream [
	"Write the JSON serialization of object to the character writeStream.
	Note that the object graph can only contain lists (Array) and maps (Dictionary)"

	"String streamContents: [ :out |
		STON put: { { #foo->1 } asDictionary. { #bar->2 } asDictionary } asJsonOnStream: out ]"

	(self jsonWriter on: writeStream) nextPut: object
]

{ #category : 'convencience' }
STON class >> put: object asJsonOnStreamPretty: writeStream [
	"Write the pretty printed JSON serialization of object to the character writeStream.
	Note that the object graph can only contain lists (Array) and maps (Dictionary)"

	"String streamContents: [ :out |
		STON put: { { #foo->1 } asDictionary. { #bar->2 } asDictionary } asJsonOnStreamPretty: out ]"

	(self jsonWriter on: writeStream)
		prettyPrint: true;
		nextPut: object
]

{ #category : 'convencience' }
STON class >> put: object onStream: writeStream [
	"Write the STON serialization of object to the character writeStream"

	"(String streamContents: [ :out | STON put: 1@2 onStream: out ]) >>> 'Point[1,2]'"

	"FileLocator temp / ('{1}.ston' format: { Time millisecondClockValue }) in: [ :file |
		file writeStreamDo: [ :out | STON put: 1@2 onStream: out ].
		file readStreamDo: [ :in | STON fromStream: in ] ]"

	(self writer on: writeStream) nextPut: object
]

{ #category : 'convencience' }
STON class >> put: object onStreamPretty: writeStream [
	"Write the pretty printed STON serialization of object to the character writeStream"

	"String streamContents: [ :out | STON put: (0@0 extent: 20@30) onStreamPretty: out ]"

	(self writer on: writeStream)
		prettyPrint: true;
		nextPut: object
]

{ #category : 'accessing' }
STON class >> reader [
	"Shortcut to create a new STONReader instance"

	^ STONReader new
]

{ #category : 'convencience' }
STON class >> toJsonString: object [
	"Return a String with the JSON serialization of object.
	Note that the object graph can only contain lists (Array) and maps (Dictionary)"

	"(STON toJsonString: { { #foo->1 } asDictionary. { #bar->2 } asDictionary }) >>> '[{""foo"":1},{""bar"":2}]'"

	^ String streamContents: [ :stream |
			self put: object asJsonOnStream: stream ]
]

{ #category : 'convencience' }
STON class >> toJsonStringPretty: object [
	"Return a String with the pretty printed JSON serialization of object.
	Note that the object graph can only contain lists (Array) and maps (Dictionary)"

	"STON toJsonStringPretty: { { #foo->1 } asDictionary. { #bar->2 } asDictionary }"

	^ String streamContents: [ :stream |
			self put: object asJsonOnStreamPretty: stream ]
]

{ #category : 'convencience' }
STON class >> toString: object [
	"Return a String with the STON serialization of object"

	"(STON toString: 1@2) >>> 'Point[1,2]'"

	^ String streamContents: [ :stream |
			self put: object onStream: stream ]
]

{ #category : 'convencience' }
STON class >> toStringPretty: object [
	"Return a String with the pretty printed STON serialization of object"

	"STON toStringPretty: (0@0 extent: 20@30)"

	^ String streamContents: [ :stream |
			self put: object onStreamPretty: stream ]
]

{ #category : 'accessing' }
STON class >> writer [
	"Shortcut to create a new STONWriter instance"

	^ STONWriter new
]
