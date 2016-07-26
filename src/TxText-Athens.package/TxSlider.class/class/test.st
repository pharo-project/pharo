tests
test
"self test"
	| m w |
	
	m := self new.
	m announcer when: TxSliderScrollForward send: #advance to: m.
	m announcer when: TxSliderScrollBackward send: #retreat to: m.
	
	w := AthensWrapMorph new.
	
	w addMorph: m.
	w openInWindow.