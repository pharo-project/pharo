*Morphic-Base-Examples
exampleWithSubMenu
	"
	self exampleWithSubMenu
	"
	| s i sub |
	s := (self withModel: nil ) label: 'Tools'. 
	i := s add: 'Tools'.
	sub := (self withModel: nil ).
	sub add: 'System browser'  target: Smalltalk tools selector: #openClassBrowser argumentList: #().
	sub add: 'Workspace'  target: Smalltalk tools selector: #openWorkspace argumentList: #().
	i subMenu: sub.
	s asMenuMorph popUpInWorld