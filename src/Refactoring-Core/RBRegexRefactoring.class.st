"
I am a abstract base class for a refactoring replacing strings by a regular expression.

My concrete subclasses define on what kind of string the replace regulare expression should be applied to.
They have to implement the RBRefactoring method #transform.

I have no special precondition.

Here is a sample of a script that will change of the YK* classes of package PrefixOfPackageNames* into ZG*.

[[[ 
| pkgPrefix newClassPrefix env model |
pkgPrefix := 'PrefixOfPackageNames'.
newClassPrefix := 'ZG'.
env := RBBrowserEnvironment new 
			forPackageNames: (RPackage organizer packageNames select: [ : pkgName | (pkgName beginsWith: pkgPrefix) ]).
model := (RBClassModelFactory rbNamespace onEnvironment: env) name: 'MyModel'; yourself.
RBClassRegexRefactoring new
	model: model;
	renameClasses; 					""Here I just want a rename no copy""
	replace: '^YK(.*)$' with: newClassPrefix,'$1';
  execute.
]]]








"
Class {
	#name : #RBRegexRefactoring,
	#superclass : #RBRefactoring,
	#instVars : [
		'matchers'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #private }
RBRegexRefactoring >> execute: aString [
	"Perform all searches on aString and return the transformation."
	
	^ matchers 
		inject: aString asString
		into: [ :string :assoc | self execute: string replace: assoc key with: assoc value ]
]

{ #category : #private }
RBRegexRefactoring >> execute: aString replace: aRegex with: aReadStream [
	| stream |
	^ aRegex copy: aString translatingMatchesUsing: [ :match |
		stream := WriteStream on: (String new: 2 * aString size).
		[ aReadStream atEnd ] whileFalse: [
			stream nextPutAll: (aReadStream upTo: $$).
			aReadStream atEnd ifFalse: [
				aReadStream peek isDigit
					ifFalse: [ stream nextPut: aReadStream next ]
					ifTrue: [ stream nextPutAll: (aRegex subexpression: aReadStream next asInteger - $0 asInteger + 1) ] ] ].
		aReadStream reset.
		stream contents ]
]

{ #category : #initialization }
RBRegexRefactoring >> initialize [
	super initialize.
	matchers := OrderedCollection new
]

{ #category : #preconditions }
RBRegexRefactoring >> preconditions [
	^ RBCondition empty
]

{ #category : #searching }
RBRegexRefactoring >> replace: aFindString with: aReplaceString [
	"Replace all matches of aFindString (regular expression) with aReplaceString, where $0 references the whole match, and $1..$9 the matched groups.
	Central method of the regex based refactoring. To be used in collaboration with renameClasses and others
	
	RBClassRegexRefactoring new
		model: model;
		renameClasses;
		replace: '^YK(.*)$' with: newClassPrefix,'$1';
 		 execute.
	Check class comment for a full script. 
	"
	self replace: aFindString with: aReplaceString ignoreCase: false
]

{ #category : #searching }
RBRegexRefactoring >> replace: aFindString with: aReplaceString ignoreCase: aBoolean [
	"Replace all matches of aFindString (regular expression) with aReplaceString, where $0 references the whole match, and $1..$9 the matched groups. See replace:with: comment too."
	
	| regex stream |
	regex := RxParser preferredMatcherClass
		for: (RxParser new parse: aFindString)
		ignoreCase: aBoolean.
	stream := aReplaceString readStream.
	matchers add: regex -> stream
]
