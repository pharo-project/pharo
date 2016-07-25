running
setUp
	super setUp.
	self class environment organization addCategory: self categoryName.
	
	td := self createTraitNamed: #TD uses: {}.		
	td compile: 'd' classified: #cat1.
	tc := self createTraitNamed: #TC uses: td.		
	tc compile: 'c' classified: #cat1.
	tb := self createTraitNamed: #TB uses: td.		
	tb compile: 'b' classified: #cat1.
	ta := self createTraitNamed: #TA uses: tb + tc @ {#cc->#c} - {#c}.
	ta compile: 'a' classified: #cat1.
	
	ca := self createClassNamed: #CA superclass: Object uses: {}.
	ca compile: 'ca' classified: #cat1.
	cb := self createClassNamed: #CB superclass: ca uses: ta.
	cb compile: 'cb' classified: #cat1.
	
	"make the class of cb also use tc:"
	cb class uses: ta classTrait + tc instanceVariableNames: ''.