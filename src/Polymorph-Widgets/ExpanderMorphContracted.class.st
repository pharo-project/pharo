"
I am an Announcement that occurs when an ExpanderMorph is contracted (rolled-up).
 
Example where visibility of another UI component depends on the expanded/contracted state of Expander 2
 
|builder expanders ex1 ex2 text list row|
builder := UITheme builder.
expanders := builder newColumn: {
 ex1 := builder newExpander: 'Expander 1'.
 ex2 := builder newExpander: 'Expander 2'}.
expanders
 width: 100;
 hResizing: #rigid.
text := builder
 newTextEditorFor: (ValueHolder new contents: 'Some text')
 getText: #contents setText: #contents:.
list := (builder
 newListFor: (ListModel new list: #('One' 'Two' 'Three' 'Four'); selectionIndex: 3)
 list: #list selected: #selectionIndex changeSelected: #selectionIndex:
 help: 'This is a list')
  minWidth: 120;
  visible: false;
  disableTableLayout: true.
ex2 announcer
 when: ExpanderMorphContracted do: [list hide; disableTableLayout: true];
 when: ExpanderMorphExpanded do: [list show; disableTableLayout: false].
row := builder newRow: {
 expanders.
 (builder newRow: {text. list})
  vResizing: #spaceFill}.
"
Class {
	#name : #ExpanderMorphContracted,
	#superclass : #ExpanderMorphAnnouncement,
	#category : #Polymorph-Widgets
}
