Extension { #name : #GTSpotter }

{ #category : #'*GT-SpotterExtensions-Core-private' }
GTSpotter >> concreteMethodsInContext: aContext usingFilter: aFilter [
	" return the concrete methods described by the expression (Class >> #Selector) - evaluate it savely "
	| classAndSelectorStrings class selector |
	classAndSelectorStrings := (aContext textTrimmed splitOn: '>>') collect: [ :each | each trimBoth ].
	
	" check preconditions "
	classAndSelectorStrings size = 2 ifFalse: [ ^ self ].
	classAndSelectorStrings second size < 2 ifTrue: [ ^ self ].

	" retrieve concrete class and selector - or saerch all behaviors "
	selector := classAndSelectorStrings second first = $# 
		ifTrue: [ classAndSelectorStrings second allButFirst asSymbol ]
		ifFalse: [ classAndSelectorStrings second asSymbol ].
	
	(class := Smalltalk classNamed: classAndSelectorStrings first) ifNil: [ 
		SystemNavigation default allBehaviorsDo: [ :aClass |
			aClass methodsDo: [ :method |
				method selector = selector ifTrue: [ aFilter value: method ] ] ].
		^ self ].
	
	
	" retrieve the compiled method in the class and class hierarchy "
	class withAllSuperclassesDo: [ :aClass |
		aFilter value: ([ aClass compiledMethodAt: selector ] on: Error do: [ ^ self ]) ]
]

{ #category : #'*GT-SpotterExtensions-Core-private' }
GTSpotter >> evaluateExpressionInContext: aSpotterContext usingFilter: filter [
	" only allow arithmethic selectors to be evaluated - otherwise this may have some severe side-effects "
	| result |
	" filter allowed selectors "
	(Number withAllSubclasses anySatisfy: [ :class |
		class selectors anySatisfy: [ :selector | 
			aSpotterContext textTrimmed includesSubstring: selector caseSensitive: true ] ]) ifFalse: [ ^ self ].
	" perform the selector "
	Smalltalk compiler 
		silentlyDo: [ result := Smalltalk compiler evaluate: aSpotterContext textTrimmed ] 
		exceptionDo: [ ^ self ].
	result isNumber ifFalse: [ ^ self ].
	filter value: result. 
	" if the result is not readable as float, append the float as well "
	(result isFraction and: [ result isInteger not ]) ifTrue: [
		filter value: result asFloat ]
]

{ #category : #'*GT-SpotterExtensions-Core-private' }
GTSpotter >> fileReferencesBy: aSelector inContext: aSpotterContext usingFilter: aBlock [
	| split rootDirectory query |
	split := aSpotterContext textLowercase piecesCutWhere: [ :a :b | a = FileSystem disk delimiter ].
	split size < 1
		ifTrue: [ 
			query := aSpotterContext textLowercase.
			rootDirectory := FileSystem workingDirectory ]
		ifFalse: [ 
			(split last last = FileSystem disk delimiter)
				ifTrue: [ 
					query := ''.
					rootDirectory := aSpotterContext textLowercase asFileReference ]
				ifFalse: [ 
					query := split last.
					rootDirectory := ('' join: split allButLast) asFileReference ] ].
	rootDirectory exists ifFalse: [ ^ self ].
	self flag: 'this is an ugly hack for #queryForFileReference. This iterator has to manipulate the query in order for the filter to work properly'.
	aSpotterContext cacheAt: #queryForFileReference put: query.
	(rootDirectory perform: aSelector) do: aBlock
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterClipboardFor: aStep [
	<spotterOrder: 90>
	^ aStep listProcessor
		title: 'Clipboard History';
		items: [ Clipboard default recentItems ];
		itemName: [ :item | item asString contractTo: 100 ];
		itemFilterName: [ :item | item asString ];
		filter: GTFilterSubstrings;
		actLogic: [ :item :step |
			step exit.
			item asString inspect ];
		wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterConcreteMethodFor: aStep [
    <spotterOrder: 30>
	self flag: #specialFilter.
	^ aStep listProcessor
		title: 'Methods';
		filter: GTNullFilter item: [ :filter :context |
			self concreteMethodsInContext: context usingFilter: filter ];
		wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterDirtyMonticelloPackagesFor: aStep [
	<spotterOrder: 85>
	aStep listProcessor
			allCandidates: [ MCWorkingCopy allManagers select: #modified ];
			title: 'Dirty Monticello packages';
			itemIcon: [ Smalltalk ui icons iconNamed: #dirtyPackageIcon ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: true
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter class >> spotterExtensionsFor: aStep [
	<spotterOrder: 50>
	aStep listProcessor
			allCandidates: [ self spotterExtendingMethods ];
			title: 'Extensions';
			itemName:[ :each | each gtDisplayString ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: true
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForCachedPlaygroundPagesFor: aStep [
	<spotterOrder: 100>
	aStep listProcessor
			allCandidates: [ GTPlayBook instance pages ];
			title: 'Playground cached pages';
			itemName: [ :page | page content asString contractTo: 100 ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForClassesFor: aStep [
	<spotterOrder: 10>
	aStep listProcessor
			allCandidates: [ Smalltalk allClassesAndTraits ];
			title: 'Classes';
			filter: GTFilterSubstring;
			itemIcon: #systemIcon;
			keyBinding: $b meta;
			wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForDirectoriesFor: aStep [
	<spotterOrder: 50>
	self flag: #specialFilter.
	aStep listProcessor
		title: 'Directories';
		itemIcon: [ GLMUIThemeExtraIcons glamorousFolder ];
		itemFilterName: [ :reference | reference basename ];
		filter: GTFilterFileReference item: [ :filter :context | 
			self fileReferencesBy: #directories inContext: context usingFilter: filter ];
		wantsToDisplayOnEmptyQuery: false	
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForExpressionsFor: aStep [
	<spotterOrder: 1>
	self flag: #specialFilter.
	aStep listProcessor
		title: 'Calculator';
		filter: GTNullFilter item: [ :filter :context | 
			self evaluateExpressionInContext: context usingFilter: filter ];
		wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForFilesFor: aStep [
	<spotterOrder: 40>
	self flag: #specialFilter.
	aStep listProcessor
		title: 'Files';
		itemIcon: [ GLMUIThemeExtraIcons glamorousBrowse ];
		itemFilterName: [ :reference | reference basename ];
		filter: GTFilterFileReference item: [ :filter :context | 
			self fileReferencesBy: #files inContext: context usingFilter: filter ];
		wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForGlobalVarsFor: aStep [
	<spotterOrder: 40>
	aStep listProcessor
			allCandidates: [ Smalltalk globals associations select:[:assoc | 
			assoc value isBehavior not and:[ 
				assoc value isTrait not]] ];
			title: 'Global variables';
			itemName:[:global | global key ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForHelpTopicFor: aStep [
	<spotterOrder: 200>
	aStep listProcessor 
			allCandidates: [ SystemHelp asHelpTopic gtAllSubtopics ];
			title: 'Help topics';
			itemName: [ :helpTopic | helpTopic title ];
			itemIcon: [ :helpTopic | helpTopic gtTopicIcon ];
			matchSubstrings;
			wantsToDisplayOnEmptyQuery: true
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForMessagesFor: aStep [
	<spotterOrder: 35>

	aStep listProcessor
		title: 'Messages';
		filter: GTNullFilter item: [ :filter :context | 
			GTSelector substring: context textTrimmed filter: filter ];
		actLogic: [ :each :step|
			step exit.
			self systemNavigation browseAllImplementorsOf: each name ];		
		wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForMetacelloConfigurationsFor: aStep [
	<spotterOrder: 86>
	aStep listProcessor
			title: 'Metacello Configurations';
			allCandidates: [ 
				Smalltalk allClasses select: [ :each | 
					(each respondsTo: #isMetacelloConfig) 
						and: [ each isMetacelloConfig
							and: [ each name beginsWith: 'ConfigurationOf' ] ] ]
					thenCollect: #project ];
			itemName: [ :each | each configuration class name ];
			itemIcon: [ Smalltalk ui icons iconNamed: #config ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForPackagesFor: aStep [
	<spotterOrder: 20>
	aStep listProcessor
			allCandidates: [ RPackageOrganizer default packages ];
			title: 'Packages';
			itemName: [ :package | package name ];
			itemIcon: [ Smalltalk ui icons iconNamed: #packageIcon ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForPragmasFor: aStep [
	<spotterOrder: 40>
	aStep listProcessor 
			allCandidates: [ PragmaType all ];
			title: 'Pragmas';
			itemName: [ :pragma | pragma keyword ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForReferencesFor: aStep [
	<spotterOrder: 32>
	
	aStep listProcessor
		title: 'References';
		filter: GTNullFilter item: [ :filter :context |
			self class environment at: context textTrimmed ifPresent: [ :class |  
				SystemNavigation default allReferencesTo: class binding do: filter ] ];
		keyBinding: $n shift meta;
		itemName: [ :method | method gtDisplayString ]
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForRepositoriesFor: aStep [
	<spotterOrder: 87>
	aStep listProcessor
		title: 'Monticello Repositories';
		allCandidates: [ MCRepositoryGroup default repositories ];
		itemIcon: #systemIcon;
		wantsToDisplayOnEmptyQuery: false;
		filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForSendersFor: aStep [
	<spotterOrder: 31>

	aStep listProcessor
		title: 'Senders';
		filter: GTNullFilter item: [ :filter :context | 
			SystemNavigation default
				allSendersToString: context textTrimmed 
				do: filter ];
		keyBinding: $n meta;
		wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterForStashedPlaygroundPagesFor: aStep [
	<spotterOrder: 100>
	aStep listProcessor
			allCandidates: [ 
				| stashFiles |
				stashFiles := GTPlayBook stashDirectory files 
					select: [ :each | each extension = GTPlayPage fileExtension ].
				stashFiles collect: [ :file |
					(GTPlayPage fromFileReference: file) 
						title: (file basename copyUpTo: $-) ] ];
			title: 'Playground named pages';
			itemName: [ :page | page title, ' - ', page content asString contractTo: 100 ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterHistoryFor: aStep [
	<spotterOrder: 8>
	aStep listProcessor
			allCandidates: [ self history asArray select: [ :each | each isNotNil and: [ each value isNotNil ] ] ];
			title: 'History';
			itemName: [ :item | 
				item class = GTSpotterProcessorLink 
					ifTrue: [ item processor itemNameFor: item value ] " HACK: this triggers a recursion ! ... but processor is undefined for aCandidate "
					ifFalse: [ item gtDisplayString ] ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterImplementorsFor: aStep [
	<spotterOrder: 30>
	self flag: #specialFilter.
	aStep listProcessor
		title: 'Implementors';
		filter: GTFilterImplementor item: [ :filter :context | 
			SystemNavigation default allBehaviorsDo: [ :class | class localMethods do: filter ] ];
		keyBinding: $m meta;
		wantsToDisplayOnEmptyQuery: false
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterStfxFor: aStep [
	<spotterOrder: 30>
	self flag: #specialFilter.
	^ aStep listProcessor
		title: 'ws.stfx.eu';
		filter: GTNullFilter item: [ :filter :context | 
			(context textTrimmed beginsWith: 'http://ws.stfx.eu')
				ifTrue: [ GTPlayPage loadFromPublishUrl: context textTrimmed thenDo: filter ] ]
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterWindowsFor: aStep [
	<spotterOrder: 5>

	aStep listProcessor
			title: 'Windows';
			candidatesLimit: 10;
			allCandidates: [ World submorphs 	select: [ :window | window isKindOf: SystemWindow ] ];
			itemName: [ :window | '{1} [{2}]' format: { window  label. window className } ];
			itemIcon: [ :window | window taskbarIcon ];
			sort:  [ :window1 :window2 | window1 label < window2 label ];
			filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: true
]

{ #category : #'*GT-SpotterExtensions-Core' }
GTSpotter >> spotterWorldMenuFor: aStep [
	<spotterOrder: 9>
	
	^ (World worldMenu spotterItemsFor: aStep)
		candidatesLimit: 15;
		allCandidates: [ World worldMenu allItems ];
		title: 'Menu'
]
