*Morphic-Base
asMenuItemMorphFrom: parentMenu isLast: aBoolean

	| it act lbl menu |
	it := self morphClass new.
	lbl := self label ifNil: [''].
	"here checked can be nil, true, false"
	checked notNil 
		ifTrue: [ lbl := self hasCheckBox -> lbl].
	it contents: lbl.
	it icon: self icon.
	it keyText: self keyText.
	it isEnabled: self enabled.
	(act := self action) ifNotNil: [
				it 
					target: act receiver;
					selector: act selector;
					arguments: act arguments ].
	(menu := self subMenu) ifNotNil: [ self enabled ifTrue: [ it subMenu: (menu asMenuMorph) ]].
		
	parentMenu ifNotNil: [ parentMenu addMorphBack: it ].
	aBoolean ifFalse: [ self separator ifTrue: [ parentMenu addLine ] ].
	^it