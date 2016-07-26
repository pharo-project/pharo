running
setUp
	"Please note, that most tests rely on this setup of traits and
	classes - and that especially the order of the definitions matters."
	"SetUpCount := SetUpCount + 1."

	dirty := false.
	SystemAnnouncer uniqueInstance suspendAllWhile: 
			[self t1: (self createTraitNamed: #T1
						uses: { }).
			self t1 comment: 'I am the trait T1'.
			
			self t1 compile: 'm11 ^11' classified: #cat1.
			self t1 compile: 'm12 ^12' classified: #cat2.
			self t1 compile: 'm13 ^self m12' classified: #cat3.
			
			self t2: (self createTraitNamed: #T2
						uses: { }).
			self t2 compile: 'm21 ^21' classified: #cat1.
			self t2 compile: 'm22 ^22' classified: #cat2.
			self t2 classSide compile: 'm2ClassSide: a ^a'.
			
			self t3: (self createTraitNamed: #T3
						uses: { }).
			self t3 compile: 'm31 ^31' classified: #cat1.
			self t3 compile: 'm32 ^32' classified: #cat2.
			self t3 compile: 'm33 ^33' classified: #cat3.
			
			self t4: (self createTraitNamed: #T4
						uses: { (self t1). (self t2) }).
			self t4 compile: 'm11 ^41' classified: #catX.	"overrides T1>>m11"
			self t4 compile: 'm42 ^42' classified: #cat2.
			
			self t5: (self createTraitNamed: #T5 uses: self t1 + self t2).
			self t5 compile: 'm51 ^super foo' classified: #cat1.
			self t5 compile: 'm52 ^ self class bar' classified: #cat1.
			self t5 compile: 'm53 ^ self class bar' classified: #cat1.
			
			self t6: (self createTraitNamed: #T6
						uses: self t1 + (self t2 @ { (#m22Alias -> #m22) })).

			self t9: (self createTraitNamed: #T9
						uses: { }).
						
			self t10: (self createTraitNamed: #T10
						uses: { }).
						
			self t12: (self createTraitNamed: #T12
						uses: {}).
	
			self t11: (self createTraitNamed: #T11
						uses: { self t12}).
			

						
			self c1: (self 
						createClassNamed: #C1
						superclass: Object
						uses: { }).
			self c1 compile: 'foo ^true' classified: #accessing.
			self c2: (self 
						createClassNamed: #C2
						superclass: self c1
						uses: self t5 - { #m11 }).
			self c2 compile: 'foo ^false' classified: #private.
			self c2 compile: 'bar ^self foo' classified: #private.
						
			self c9: (self 
						createClassNamed: #C9
						superclass: Object
						uses: self t9 ).
						
			self c10: (self 
						createClassNamed: #C10
						superclass: self c9
						uses: self t10 ).
						
			self c11: (self 
						createClassNamed: #C11
						superclass: self c10
						uses: self t11 ).
			
			self setUpTrivialRequiresFixture.
			self setUpTwoLevelRequiresFixture.
			self setUpTranslatingRequiresFixture].