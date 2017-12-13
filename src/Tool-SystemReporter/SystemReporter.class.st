"
SystemReporter offers a window where information about the system is gathered. This can be easily copied to the clipboard and be attached to a bug report for better identification of the context the bug occured in.

To extend the SystemReporter:
	- add a method
		reportXYZ: aStream
	  to the reporting category
	- insert a line
		add: #XYZ method: #reportXYZ
	  to the initialize method

"
Class {
	#name : #SystemReporter,
	#superclass : #Model,
	#instVars : [
		'categories',
		'categoriesSelected',
		'report',
		'categoryList',
		'sourceTextModel',
		'spaceAnalysisResult'
	],
	#category : #'Tool-SystemReporter'
}

{ #category : #'instance creation' }
SystemReporter class >> open [

	^ self new open
]

{ #category : #'system menu' }
SystemReporter class >> systemReporterMenuOn: aBuilder [ 
	"I build a menu"
	<worldMenu>
	(aBuilder item: #'System Reporter')
		parent: #System;
		action: [ self open ];
		help: 'If you have a bug, use this tool to provide information about your system.'.
]

{ #category : #operations }
SystemReporter >> add: category method: aSymbol [
	^self categoryList add: (self categories add: category -> aSymbol) key
]

{ #category : #display }
SystemReporter >> addAll: aWindow [

	aWindow
		addMorph: self buildInformationList
		fullFrame: self categoriesFrame.
		
	aWindow
		addMorph: self buildInformationText
		fullFrame: self reportFrame.
]

{ #category : #'items creation' }
SystemReporter >> buildInformationList [

	^ PluggableListMorph
		on: self
		list: #categoryList
		primarySelection: #categorySelected
		changePrimarySelection: #categorySelected:
		listSelection: #categoryAt:
		changeListSelection: #categoryAt:put:
		menu: nil
		keystroke: nil
]

{ #category : #'items creation' }
SystemReporter >> buildInformationText [

	| scrolledText |
	scrolledText := self sourceTextModel newScrolledText
		beForPlainText;
		beReadOnly;
		beWrapped;
		yourself.
	^ scrolledText
]

{ #category : #accessing }
SystemReporter >> categories [
	^ categories ifNil: [categories := IdentityDictionary new]
]

{ #category : #display }
SystemReporter >> categoriesFrame [
	^ LayoutFrame new rightFraction: 0.25
		
]

{ #category : #'accessing-categories' }
SystemReporter >> categoryAt: anIndex [
	^ categoriesSelected includes: (self categoryList at: anIndex ifAbsent: [ ^ false ]).
]

{ #category : #'accessing-categories' }
SystemReporter >> categoryAt: anInteger put: aBoolean [
	categoriesSelected := categoriesSelected
		perform: (aBoolean ifTrue: [ #copyWith: ] ifFalse: [ #copyWithout: ])
		with: (self categoryList at: anInteger ifAbsent: [ ^ self ]).
	self updateReport
]

{ #category : #accessing }
SystemReporter >> categoryList [
	^ categoryList ifNil: [categoryList := OrderedCollection new]
]

{ #category : #'accessing-categories' }
SystemReporter >> categoryMenu: aMenu [
	^ aMenu
		addTitle: 'Categories';
		add: 'Select all' selector: #selectAllCategories;
		add: 'Select none' selector: #selectNoCategories;
		addLine;
		add: 'Refresh' selector: #refresh;
		yourself
]

{ #category : #'accessing-categories' }
SystemReporter >> categorySelected [
	^ 0
]

{ #category : #'accessing-categories' }
SystemReporter >> categorySelected: anInteger [

	self changed: #categorySelected.
]

{ #category : #'accessing-report' }
SystemReporter >> copyReportToClipboard [
	Clipboard clipboardText: self reportText.
	UIManager default inform: 'Copied Report to Clipboard'
]

{ #category : #private }
SystemReporter >> enumerate: aBlock on: aStream [
	self enumerate: aBlock startAt: 0 on: aStream
]

{ #category : #private }
SystemReporter >> enumerate: aBlock startAt: first on: aStream [
	"Utilitymethod to enumerate Options or Parameters from first to 1000"

	| idx value |
	idx := first.
	[ 
	value := aBlock value: idx.
	value isNil or: [ idx > 1000 ] ]
		whileFalse: [ 
			aStream
				nextPut: $#;
				nextPutAll: idx printString;
				tab;
				nextPutAll: value;
				cr.
			idx := idx + 1 ].
	idx = first
		ifTrue: [ 
			aStream
				nextPutAll: 'none';
				cr ]
]

{ #category : #'accessing-ui' }
SystemReporter >> extent [
	^ 640 @ 480
]

{ #category : #'printing-report' }
SystemReporter >> header: aString on: aStream [
	aStream withAttribute: TextEmphasis bold do: [	
		aStream nextPutAll: aString; cr.
		aString size timesRepeat: [aStream nextPut: $-].
		aStream cr]
]

{ #category : #'accessing-ui' }
SystemReporter >> initialExtent [
	^ 640 @ 480
]

{ #category : #initialization }
SystemReporter >> initialize [
	super initialize.
	self
		add: #Image method: #reportImage;
		add: #'Image Parameters' method: #reportImageParameters;
		add: #'Image Sources' method: #reportSources;
		add: #'MC Repositories' method: #reportRepositories;
		add: #'MC Working Copies' method: #reportWorkingCopies;
		add: #'VM General' method: #reportVM;
		add: #'VM Options' method: #reportVMOptions;
		add: #'VM Modules' method: #reportModules;
		add: #'VM Parameters' method: #reportVMParameters;
		add: #'VM Stats' method: #reportVMStats;
		add: #'Space Analysis' method: #reportSpaceAnalysis.
	Smalltalk os platformName = 'Win32'
		ifTrue: [ self add: #'VM Configuration' method: #reportWin32VMConfig ].
	self add: #'OS General' method: #reportOS.
	Smalltalk os platformName = 'Win32'
		ifTrue: [ 
			self
				add: #'OS Details' method: #reportWin32OSDetails;
				add: #'Hardware Details' method: #reportWin32HardwareDetails;
				add: #'GFX Hardware Details' method: #reportWin32GFXDetails ].
	Smalltalk os version = 'linux'
		ifTrue: [ self add: #'OS Details' method: #reportLinuxOSDetails ].
	self add: #'OS Environment' method: #reportOSEnvironment.
	categoriesSelected := Set with: #Image with: #'VM General'.
	self updateReport
]

{ #category : #'accessing-ui' }
SystemReporter >> label [
	^ 'System Reporter' 
]

{ #category : #display }
SystemReporter >> open [
	"self new open"
	
	| window |
	window := StandardWindow new model: self.		
	self addAll: window.
	window openInWorld.
	window title: self label.
	^ window			
]

{ #category : #operations }
SystemReporter >> refresh [
	self updateReport
]

{ #category : #display }
SystemReporter >> reportFrame [
	^ LayoutFrame new leftFraction: 0.25


]

{ #category : #reporting }
SystemReporter >> reportImage: aStream [
	self header: 'Image' on: aStream.
	aStream 
		nextPutAll: Smalltalk image imagePath; cr;
		nextPutAll: SystemVersion current version; cr;
		nextPutAll: Smalltalk image lastUpdateString; cr;
		nextPutAll: ChangeSet current name; cr

]

{ #category : #reporting }
SystemReporter >> reportImageParameters: aStream [
	self header: 'Image Commandline Parameters' on: aStream.
	self enumerate: [:idx | Smalltalk image argumentAt: idx] on: aStream.
]

{ #category : #reporting }
SystemReporter >> reportLinuxOSDetails: aStream [
	self header: 'Operating System Details' on: aStream.
	#(
		'/etc/issue'
		'/etc/lsb-release'
		'/proc/version'
	) do: [:path|
		self writeContentsSafelyFromFile: path on: aStream]
]

{ #category : #'accessing-report' }
SystemReporter >> reportMenu: aMenu [
	^ aMenu
		addTitle: 'Report' ;
		add: 'Copy to Clipboard' selector: #copyReportToClipboard;
		yourself
]

{ #category : #reporting }
SystemReporter >> reportModules: aStream [
	self header: 'Loaded VM Modules' on: aStream.
	Smalltalk vm listLoadedModules asSortedCollection do: [:each | aStream nextPutAll: each; cr].
	aStream cr.
	self header: 'VM Built-in Modules' on: aStream.
	Smalltalk vm listBuiltinModules asSortedCollection do: [:each | aStream nextPutAll: each; cr].


]

{ #category : #reporting }
SystemReporter >> reportOS: aStream [
	self header: 'Operating System/Hardware' on: aStream.
	aStream
		nextPutAll: Smalltalk os platformName; space;
		nextPutAll: Smalltalk os version; space;
		nextPutAll: Smalltalk os subtype; cr

]

{ #category : #reporting }
SystemReporter >> reportOSEnvironment: aStream [
	| env |
	self header: 'Operating System Environment' on: aStream.
	env := [Smalltalk os environment] on: Error do: [^self].
	env keys asSortedCollection do: [:name |
		aStream nextPutAll: name; nextPut: $=; nextPutAll: (env at: name); cr]
	
]

{ #category : #reporting }
SystemReporter >> reportRepositories: aStream [
	self header: 'Monticello Repositories' on: aStream.
	MCRepositoryGroup default repositories do: [:each | aStream nextPutAll: each description; cr]
]

{ #category : #reporting }
SystemReporter >> reportSources: aStream [
	self header: 'Image Sources' on: aStream.
	aStream nextPutAll: SourceFiles class printString; cr.
	aStream nextPutAll: SourceFiles reportString.
]

{ #category : #reporting }
SystemReporter >> reportSpaceAnalysis: aStream [

	spaceAnalysisResult ifNil: [
		(UIManager default
			confirm: 'Running the space analysis\might take a few seconds.' withCRs
			label: 'System Reporter')
				ifFalse: [
					categoriesSelected remove: #'Space Analysis'.
					^ self changed: #categorySelected ].
		
		spaceAnalysisResult := String streamContents: [ :stream |
			SpaceTally new printSpaceAnalysis: 1 onStream: stream ] ].
	self header: 'Space Analysis' on: aStream.
	aStream nextPutAll: spaceAnalysisResult; cr
]

{ #category : #'accessing-report' }
SystemReporter >> reportText [

	^ (report isNil or: [categoriesSelected isEmpty])
		ifTrue: ['-- Choose any category on the left --']
		ifFalse: [report]
]

{ #category : #reporting }
SystemReporter >> reportVM: aStream [
	self header: 'Virtual Machine' on: aStream.
	aStream
		nextPutAll: Smalltalk vm vmFileName;
		cr;
		nextPutAll: Smalltalk vm version;
		cr.
	Smalltalk vm buildDate
		ifNotNil: [ :string | 
			aStream
				nextPutAll: string;
				cr ].
	[ 
	Smalltalk vm platformSourceVersion
		ifNotNil: [ :v | 
			aStream
				nextPutAll: 'platform sources revision ';
				nextPutAll: v;
				cr ] ]
		on: Warning
		do: [ 
			"unsupported primitive"
			 ].
	[ 
	Smalltalk vm interpreterSourceVersion
		ifNotNil: [ :v | 
			aStream
				nextPutAll: 'VMMaker versionString ';
				nextPutAll: v;
				cr ] ]
		on: Warning
		do: [ 
			"unsupported primitive"
			 ].
	[ 
	Smalltalk vm interpreterClass
		ifNotNil: [ :string | 
			aStream
				nextPutAll: string;
				cr ].
	Smalltalk vm cogitClass
		ifNotNil: [ :string | 
			aStream
				nextPutAll: string;
				cr ] ]
		on: Error
		do: [ 
			"unsupported primitives"
			 ]
]

{ #category : #reporting }
SystemReporter >> reportVMOptions: aStream [
	self header: 'Virtual Machine Commandline Options' on: aStream.
	self enumerate: [:idx | Smalltalk vm optionAt: idx] startAt: 1 on: aStream
]

{ #category : #reporting }
SystemReporter >> reportVMParameters: aStream [
	| vmParameters |
	self header: 'Virtual Machine Parameters' on: aStream.
	vmParameters := Smalltalk vm getParameters.
	#(
		1 'end (v3)/size(Spur) of old-space (0-based, read-only)'
		2 'end (v3)/size(Spur) of young/new-space (read-only)'
		3 'end (v3)/size(Spur) of heap (read-only)'
		4 'nil (was allocationCount (read-only))'
		5 'nil (was allocations between GCs (read-write)'
		6 'survivor count tenuring threshold (read-write)'
		7 'full GCs since startup (read-only)'
		8 'total milliseconds in full GCs since startup (read-only)'
		9 'incremental GCs (SqueakV3) or scavenges (Spur) since startup (read-only)'
		10 'total milliseconds in incremental GCs (SqueakV3) or scavenges (Spur) since startup (read-only)'
		11 'tenures of surving objects since startup (read-only)'
		12 '12-20 were specific to ikp''s JITTER VM, now 12-19 are open for use'
		13 '12-20 were specific to ikp''s JITTER VM, now 12-19 are open for use'
		14 '12-20 were specific to ikp''s JITTER VM, now 12-19 are open for use'
		15 '12-20 were specific to ikp''s JITTER VM, now 12-19 are open for use'
		16 '12-20 were specific to ikp''s JITTER VM, now 12-19 are open for use'
		17 '12-20 were specific to ikp''s JITTER VM, now 12-19 are open for use'
		18 '12-20 were specific to ikp''s JITTER VM, now 12-19 are open for use'
		19 '12-20 were specific to ikp''s JITTER VM, now 12-19 are open for use'
		20	'utc microseconds at VM start-up (actually at time initialization, which precedes image load).'
		21	'root table size (read-only)'
		22	'root table overflows since startup (read-only)'
		23	'bytes of extra memory to reserve for VM buffers, plugins, etc (stored in image file header).'
		24	'memory threshold above which shrinking object memory (rw)'
		25	'memory headroom when growing object memory (rw)'
		26	'interruptChecksEveryNms - force an ioProcessEvents every N milliseconds	(rw) 27	number of times mark loop iterated for current IGC/FGC (read-only)	includes ALL marking'
		28	'number of times sweep loop iterated for current IGC/FGC (read-only)'
		29	'number of times make forward loop iterated for current IGC/FGC	(read-only) 30	number of times compact move loop iterated for current	IGC/FGC (read-only)'
		31	'number of grow memory requests (read-only)'
		32	'number of shrink memory requests (read-only)'
		33	'number of root table entries used for current IGC/FGC (read-only)'
		34	'number of allocations done before current IGC/FGC (read-only)'
		35	'number of survivor objects after current IGC/FGC (read-only)'
		36	'millisecond clock when current IGC/FGC completed (read-only)'
		37	'number of marked objects for Roots of the world, not including Root	Table entries for current IGC/FGC (read-only)'
		38	'milliseconds taken by current IGC (read-only)'
		39	'Number of finalization signals for Weak Objects pending when current	IGC/FGC completed (read-only)'
		40	'BytesPerOop for this image'
		41	'imageFormatVersion for the VM'
		42	'number of stack pages in use'
		43	'desired number of stack pages (stored in image file header, max 65535)'
		44	'size of eden, in bytes'
		45	'desired size of eden, in bytes (stored in image file header)'
		46	'machine code zone size, in bytes (Cog only; otherwise nil)'
		47	'desired machine code zone size (stored in image file header; Cog only;	otherwise nil)'
		48	'various header flags. See getCogVMFlags.'
		49	'max size the image promises to grow the external semaphore table to (0	sets to default, which is 256 as of writing)'
		50	'nil; reserved for VM parameters that persist in the image (such as	eden above)'
		51 	'nil; reserved for VM parameters that persist in the image (such as	eden above)'
		52	'root table capacity'
		53	'number of segments (Spur only; otherwise nil)'
		54	'total size of free old space (Spur only, otherwise nil)'
		55	'ratio of growth and image size at or above which a GC will be performed	post scavenge'
		56	'number of process switches since startup (read-only)'
		57	'number of ioProcessEvents calls since startup (read-only)'
		58	'number of ForceInterruptCheck calls since startup (read-only)'
		59	'number of check event calls since startup (read-only)'
		60	'number of stack page overflows since startup (read-only)'
		61	'number of stack page divorces since startup (read-only)	62	compiled code compactions since startup (read-only; Cog only; otherwise nil)'
		63	'total milliseconds in compiled code compactions since startup	(read-only; Cog only; otherwise nil)'
		64	'the number of methods that currently have jitted machine-code'
		65	'whether the VM supports a certain feature, MULTIPLE_BYTECODE_SETS is bit 0, IMMTABILITY is bit 1'
		66	'the byte size of a stack page'
		67	'the max allowed size of old space (Spur only; nil otherwise; 0 implies	no limit except that of the underlying platform)'
		68	'the average number of live stack pages when scanned by GC (at	scavenge/gc/become et al)'
		69	'the maximum number of live stack pages when scanned by GC (at	scavenge/gc/become et al)'
		70	'the vmProxyMajorVersion (the interpreterProxy VM_MAJOR_VERSION)'
		71	'the vmProxyMinorVersion (the interpreterProxy VM_MINOR_VERSION)')
			 pairsDo: [:idx :desc |
		aStream
			nextPut: $#;
			nextPutAll: idx printString;
			tab;
			nextPutAll: (vmParameters at: idx) printString;
			tab;
			nextPutAll: desc;
			cr]
]

{ #category : #reporting }
SystemReporter >> reportVMStats: aStream [
	self header: 'Virtual Machine Statistics' on: aStream.
	aStream
		nextPutAll: Smalltalk vm statisticsReport
]

{ #category : #reporting }
SystemReporter >> reportWin32GFXDetails: aStream [
	self header: 'Graphics Hardware Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10003)
]

{ #category : #reporting }
SystemReporter >> reportWin32HardwareDetails: aStream [
	self header: 'Hardware Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10001)
]

{ #category : #reporting }
SystemReporter >> reportWin32OSDetails: aStream [
	self header: 'Operating System Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10002)
]

{ #category : #reporting }
SystemReporter >> reportWin32VMConfig: aStream [
	| exePath iniPath |
	self header: 'VM Configuration' on: aStream.
	exePath := Smalltalk vm vmFileName.
	iniPath := (exePath copyUpToLast: $.), '.ini'.
	aStream nextPutAll: iniPath; cr.
	self writeContentsSafelyFromFile: iniPath on: aStream.

]

{ #category : #reporting }
SystemReporter >> reportWorkingCopies: aStream [
	| list |
	self header: 'Monticello Working Copies' on: aStream.
	list := MCWorkingCopy allManagers asSortedCollection: [:a :b | a packageName <= b packageName]  .
	list do: [:each | aStream nextPutAll: each description; cr]
]

{ #category : #operations }
SystemReporter >> selectAllCategories [
	categoriesSelected addAll: categoryList.
	self changed: #categorySelected.
	self updateReport
]

{ #category : #operations }
SystemReporter >> selectNoCategories [
	categoriesSelected removeAll.
	self changed: #categorySelected.
	self updateReport
]

{ #category : #accessing }
SystemReporter >> sourceTextModel [
	^ sourceTextModel ifNil: [ sourceTextModel := RubScrolledTextModel new interactionModel: self ].
	
]

{ #category : #updating }
SystemReporter >> updateReport [
	self sourceTextModel setText: (Text streamContents: [:stream | 
						self categoryList do: [:each |
							(categoriesSelected includes: each) ifTrue: [
								self perform: ((categories at: each), ':') asSymbol with: stream.
								stream cr]]])

]

{ #category : #private }
SystemReporter >> writeContentsSafelyFromFile: osPath on: aStream [
	aStream nextPutAll:
		([
			(FileStream readOnlyFileNamed: osPath) upToEnd
		 ] on: Error do: [:ex| ex return: ex printString])
]
