pharo - updates
update60460
	"self new update60460"
	self withUpdateLog: '18028 Playground play-cache does ignore rename of current working directory
	https://pharo.fogbugz.com/f/cases/18028

19895 Remove SmallInteger>>#instVarAt:
	https://pharo.fogbugz.com/f/cases/19895

19855 SyntaxErrorNotification skips ensured block
	https://pharo.fogbugz.com/f/cases/19855

19916 Fix correct author naming
	https://pharo.fogbugz.com/f/cases/19916'.
	self loadTogether: self script60460 merge: false.
	
[| authors blk |
	
	authors := Dictionary new.
	blk := [ :mtd | 
		mtd timeStamp ifNotEmpty: [ :stamp |
			(' ' join: ((stamp splitOn: ' ') allButLast: 2)) ifNotEmpty: [ :author |
				 author trimmed ifNotEmpty: [ 
					authors at: author 
						ifPresent: [ :col | col add: mtd ]
						ifAbsentPut: [ OrderedCollection with: mtd ]
					]
			]
		] 
	].
	
	Smalltalk allClassesAndTraitsDo: [ :each |
		each methodsDo: blk. 
		each theMetaClass methodsDo: blk.
	].

Author useAuthor: 'HenrikNergaard' during: [ 
	#(pbCompiler hn Latsabben HenrikNergrd) do: [ :old |
		(authors at: old) do: [ :mtd |
			mtd methodClass 
				compile: mtd sourceCode 
				classified: mtd protocol
		]	
	]
]
] value.
	self flushCaches.
