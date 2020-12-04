"
I am a complex processor that unifies queries from: Classes, Implementors and Packages.
I can handle filtering by categories, so to have different behavior when the query includes #Classes, #Packages, #Implementors.
I configure the keymapping for the categories in spotter.

My source is a complex one: 
 
 - I will filter the duplicates out.
 - I will configure it using the categories in the query.
 - I have two behaviors, depending of the case of query. If it is uppercase, I will search first in implementors, then in classes and then in packages. If it is lowercase I will search in classes, packages and then implementors.
 - I first search for matches starting with the query and then for matches with the query in any place of the entry.
"
Class {
	#name : #SptUnifiedProcessor,
	#superclass : #SptSpotterProcessor,
	#instVars : [
		'categories'
	],
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptUnifiedProcessor class >> defaultEnabled [

	^ true
]

{ #category : #accessing }
SptUnifiedProcessor class >> order [
	
	^ 10
]

{ #category : #settings }
SptUnifiedProcessor class >> settingsDescription [

	^ 'I am the main processor: I show classes, packages and implementors'
]

{ #category : #settings }
SptUnifiedProcessor class >> settingsTitle [

	^ 'Classes, Implementors & Packages'
]

{ #category : #accessing }
SptUnifiedProcessor class >> title [

	^ 'Results'
]

{ #category : #sources }
SptUnifiedProcessor >> calculateLowercaseSourcesOn: anStream [

	| implementorsBeginsWith implementorsSubstring wordsAwareImplementors |

	(self categoriesIncludes: #Implementors) 
		ifFalse: [ ^ self ].

	implementorsBeginsWith := SptSourceFactory current implementorsBeginsWithSource.
	implementorsSubstring := SptSourceFactory current implementorsSubstringSource.
	wordsAwareImplementors := SptWordsAwareFilter new 
		inner: (SptSourceFactory current implementorsSubstringSource); 
		yourself.
	
	textFilteringIterators add: implementorsBeginsWith.
	textFilteringIterators add: implementorsSubstring.
	textFilteringIterators add: wordsAwareImplementors.
	
	anStream nextPut: implementorsBeginsWith.
	anStream nextPut: implementorsSubstring.
	anStream nextPut: wordsAwareImplementors 

]

{ #category : #sources }
SptUnifiedProcessor >> calculateUppercaseSourcesOn: anStream [

	| classesBeginsWith classesIncludesString packagesBeginsWith packagesIncludesString wordsAwareClassesSource wordsAwarePackageSource classAndSelectorImplementors |
	
	(self categoriesIncludes: #Classes) 
		ifTrue: [
			classesBeginsWith := SptSourceFactory current classesBeginsWithSource.			
			textFilteringIterators add: classesBeginsWith.
			anStream nextPut: classesBeginsWith ].

	(self categoriesIncludes: #Packages) 
		ifTrue: [
			packagesBeginsWith := (SptPackageIterator new collect: [ :e | SptPackageEntry on: e ]) asBeginsWithFilter.			
			textFilteringIterators add: packagesBeginsWith.
			anStream nextPut: packagesBeginsWith ].

	(self categoriesIncludes: #Classes) 
		ifTrue: [
			classesIncludesString := SptSourceFactory current classesSubstringSource.
			textFilteringIterators add: classesIncludesString.
			anStream nextPut: classesIncludesString ].
		
	(self categoriesIncludes: #Packages) 
		ifTrue: [
			packagesIncludesString := (SptPackageIterator new collect: [ :e | SptPackageEntry on: e ]) asSubstringFilter.		
			textFilteringIterators add: packagesIncludesString.
			anStream nextPut: packagesIncludesString].
		
	(self categoriesIncludes: #Classes) 
		ifTrue: [
			wordsAwareClassesSource := SptWordsAwareFilter new inner: (SptSourceFactory current classesSubstringSource); yourself.
			textFilteringIterators add: wordsAwareClassesSource.
			anStream nextPut: wordsAwareClassesSource ].

	(self categoriesIncludes: #Implementors) 
		ifTrue: 	[ 	
			classAndSelectorImplementors := SptClassWithSelectorWordFilter new 
				inner: (SptSourceFactory current implementorsSubstringSource); 
				yourself.
			textFilteringIterators add: classAndSelectorImplementors.
			anStream nextPut: classAndSelectorImplementors].

	(self categoriesIncludes: #Packages) 
		ifTrue: [
			wordsAwarePackageSource := SptWordsAwareFilter new inner: ((SptPackageIterator new collect: [ :e | SptPackageEntry on: e ]) asSubstringFilter); yourself.
			textFilteringIterators add: wordsAwarePackageSource.
			anStream nextPut: wordsAwarePackageSource ].

]

{ #category : #sources }
SptUnifiedProcessor >> categoriesIncludes: aCategory [

	^ categories isNil or: [ categories isEmpty or: [ categories includes: aCategory ] ] 
]

{ #category : #filtering }
SptUnifiedProcessor >> configureFilter [

	(categories ~= self query categoriesSelected)
		ifTrue: [
			categories := self query categoriesSelected.
			filter := nil].

	super configureFilter.
	
	"I will clear the duplicates, as a new query have done"
	filter clearDuplicates.
	

]

{ #category : #'key-bindings' }
SptUnifiedProcessor >> installKeymappingsOn: aSpotterMorph [

	spotterMorph := aSpotterMorph.

	aSpotterMorph kmDispatcher
				bindKeyCombination: $b meta
				toAction: (MessageSend receiver: self query selector: #filterClasses).

	aSpotterMorph kmDispatcher
				bindKeyCombination: $m meta
				toAction: (MessageSend receiver: self query selector: #filterImplementors).
	
]

{ #category : #'spotter-api' }
SptUnifiedProcessor >> isRelevantForQuery: aString [ 
	
	"I will handle... I am old enough"
	^ true
]

{ #category : #filtering }
SptUnifiedProcessor >> newSource [

	| uppercaseIterators lowercaseIterators caseSelectorIterator root |
	textFilteringIterators := OrderedCollection new.

	lowercaseIterators := OrderedCollection streamContents: [ :s | self calculateLowercaseSourcesOn: s].
	uppercaseIterators := OrderedCollection streamContents: [ :s | self calculateUppercaseSourcesOn: s].

	caseSelectorIterator := SptCaseSelectorIterator new 
		lowercaseIterator: (SptSequenceIterator with: lowercaseIterators);
		uppercaseIterator: (SptSequenceIterator with: uppercaseIterators).
		
	textFilteringIterators add: caseSelectorIterator.
	
	root := caseSelectorIterator asWithoutDuplicates.
		
	^ root



]
