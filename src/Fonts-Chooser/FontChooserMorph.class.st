"
I'm the UI of a font chooser. 

FontChooser 
	openWithWindowTitle: 'Choose the Menu Font' 
	for: StandardFonts 
	setSelector: #menuFont: 
	getSelector: #menuFont
	

"
Class {
	#name : #FontChooserMorph,
	#superclass : #StandardWindow,
	#instVars : [
		'fontPreviewPanel',
		'okButton',
		'cancelButton',
		'applyButton',
		'updateButton',
		'result',
		'pointSizeMorph',
		'styleList',
		'pointSizeList',
		'selectedPangram',
		'languageList',
		'fontList'
	],
	#category : #'Fonts-Chooser'
}

{ #category : #'instance creation' }
FontChooserMorph class >> withModel: aFontChooser [
	^self new 
		initializeWithModel: aFontChooser;
		yourself.
]

{ #category : #actions }
FontChooserMorph >> apply [
	result := model selectedFont.
	model apply
]

{ #category : #'ui elements' }
FontChooserMorph >> applyButton [
	^applyButton ifNil: [
		applyButton := (self
			newButtonFor: self
			action: #applyButtonClicked
			label: 'Apply' translated
			help: 'Click here to apply your selection without closing this dialog' translated)
			hResizing: #spaceFill ]
]

{ #category : #actions }
FontChooserMorph >> applyButtonClicked [
	self apply.
	
]

{ #category : #'ui elements' }
FontChooserMorph >> cancelButton [
	^cancelButton ifNil: [
		cancelButton := (self
			newButtonFor: self
			action: #cancelButtonClicked
			label: 'Cancel' translated
			help: 'Click here to cancel and close this dialog' translated)
			hResizing: #spaceFill ]
]

{ #category : #actions }
FontChooserMorph >> cancelButtonClicked [
	result :=nil.
	self delete
	
]

{ #category : #'interface building' }
FontChooserMorph >> createWindow [
	"Create the package loader window."
	
	| buttonBar buttonBarOffset previewFrame buttonBarFrame |	
	buttonBar := self newRow: {
		self applyButton.
		self okButton.
		self cancelButton.
		self updateButton }.
	buttonBar cellInset: 8@0.
	buttonBarOffset := buttonBar minExtent y negated - (2 * ProportionalSplitterMorph splitterWidth).
	
	fontList := self newFontList borderWidth: 0.
	self 
		addMorph: fontList
		frame: (0.0 @ 0.0 corner: 0.4 @ 0.4).

	styleList := self newFontStyleList
		borderWidth: 0;
		yourself.
	self 
		addMorph: styleList
		frame: (0.4 @ 0.0 corner: 0.6 @ 0.4).

	pointSizeList := self newPointSizeList
		borderWidth: 0;
		yourself.
	self 
		addMorph: pointSizeList
		frame: (0.6 @ 0.0 corner: 0.75 @ 0.4).
		
	languageList := self newLanguageList
		borderWidth: 0;
		yourself.
	self 
		addMorph: languageList
		frame: (0.75 @ 0.0 corner: 1.0 @ 0.4).
	self languageIndex: 1.	
	
	previewFrame := (0 @ 0.4 corner: 1 @1) asLayoutFrame
		bottomOffset: buttonBarOffset
		yourself.
	self 
		addMorph: self fontPreviewPanel
		fullFrame: previewFrame. 

	buttonBarFrame := (0 @1 corner: 1@1) asLayoutFrame
		topOffset: buttonBarOffset;
		yourself.
	self 
		addMorph: buttonBar
		fullFrame: buttonBarFrame.
]

{ #category : #'open/close' }
FontChooserMorph >> delete [
	model := nil.
	super delete
	
]

{ #category : #'ui elements' }
FontChooserMorph >> fontPreviewPanel [ 

	^fontPreviewPanel ifNil:
		[ fontPreviewPanel := self newScrollPaneFor: self newFontPreviewInnerPanel ]
]

{ #category : #initialization }
FontChooserMorph >> initializeLabelArea [
	"Customize the window bar by removing all the decorations"
	
	super initializeLabelArea.
	self
		removeCollapseBox;
		removeExpandBox;
		removeMenuBox
]

{ #category : #initialization }
FontChooserMorph >> initializeWithModel: aFontChooser [
	self 
		model: aFontChooser;
		clipSubmorphs: true;
		setLabel: self model windowTitle;
		name: 'FontChooser'.
	self model announcer when: #pointSizeListChanged send: #updatePointSizeList to: self.
	self model announcer when: #selectedFontIndex send: #updateFontIndex to: self.
	self model announcer when: #pointSizeChanged send: #updatePointSize to: self.
	self model announcer when: #selectedFontStyleIndex send: #updateStyleList to: self.
	self updatePreview
]

{ #category : #accessing }
FontChooserMorph >> languageIndex [

	| item |
	item := self pangrams detect: [ :one | one value = selectedPangram ] ifNone: nil.
	^self pangrams indexOf: item.
]

{ #category : #accessing }
FontChooserMorph >> languageIndex: anIndex [

	anIndex isZero ifTrue: [ ^ self ]. 
	selectedPangram := (self pangrams at: anIndex) value.
	languageList selectionIndex: anIndex.
	self updatePreview
]

{ #category : #'interface building' }
FontChooserMorph >> languages [
	
	^ self pangrams collect: #key
]

{ #category : #'interface building' }
FontChooserMorph >> newFontList [ 

	| widget requiredWidth |
	widget := PluggableListMorph
		on: self model
		list: #fontListStrings
		selected: #selectedFontIndex
		changeSelected: #selectedFontIndex:.

	requiredWidth := self
		widthRequiredFor: [ :each | widget font widthOfStringOrText: each familyName ]
		from: model fontList
		noLessThan: 20.

	widget	
		color: Color white;
		borderInset;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		width: requiredWidth + widget scrollBarThickness + (widget font widthOfStringOrText: '  ').
		
	self model announcer when: #fontsUpdated send: #verifyContents to: widget.
	
	^ widget.
]

{ #category : #'interface building' }
FontChooserMorph >> newFontPointSizeField [
	^ (RubScrolledTextMorph
		on: self
		text: #pointSizeString
		accept: #pointSizeString:)
		acceptOnCR: true;
		vScrollbarShowNever;
		color: Color gray veryMuchLighter;
		borderColor: #inset;
		vResizing: #rigid;
		hResizing: #spaceFill;
		width:
			(TextStyle defaultFont
				widthOfString: '99999999.99');
		height:
			TextStyle defaultFont height + 6;
		yourself
]

{ #category : #'interface building' }
FontChooserMorph >> newFontPointSizeLabel [ 

	^ StringMorph
		contents: 'Point size:' translated;
		yourself.
]

{ #category : #'interface building' }
FontChooserMorph >> newFontPreviewInnerPanel [
	"Answer a morph for the preview text."
	
	| textMorph |
	textMorph := self newText: self previewText.
	textMorph margins: 4 asMargin.
	^ self model selectedFont
		ifNil: [ textMorph ]
		ifNotNil: [ :f | textMorph beAllFont: f ]
]

{ #category : #'interface building' }
FontChooserMorph >> newFontSizePanel [

	pointSizeMorph := self newFontPointSizeField.
	^ Morph new
		borderWidth: 1;
		borderColor: Color black;
		hResizing: #spaceFill;
		vResizing: #shrinkwrap;
		color: Color transparent;
		layoutPolicy: TableLayout new;
		cellInset: 0;
		listCentering: #topLeft;
		listDirection: #leftToRight;
		cellPositioning: #leftCenter;
		clipSubmorphs: true;
		addMorphBack: self newFontPointSizeLabel;
		addMorphBack: pointSizeMorph.
		
]

{ #category : #'interface building' }
FontChooserMorph >> newFontStyleList [ 

	| widget minWidth requiredWidth |
	widget := PluggableListMorph
		on: self model
		list: #fontStyleListStrings
		selected: #selectedFontStyleIndex
		changeSelected: #selectedFontStyleIndex:.
		
	minWidth := widget font widthOfStringOrText: 'Condensed Extra Bold Oblique' "long, but not the longest".
	requiredWidth := self
		widthRequiredFor: [ :each | widget font widthOfStringOrText: each styleName ]
		from: model fontStyleList
		noLessThan: minWidth.

	^ widget	
			color: Color white;
			borderInset;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			width: requiredWidth + widget scrollBarThickness + (widget font widthOfStringOrText: '  ');
			yourself.
]

{ #category : #'interface building' }
FontChooserMorph >> newLanguageList [

	| widget |
	widget := PluggableListMorph
		on: self 
		list: #languages
		selected: #languageIndex
		changeSelected: #languageIndex:.
	^ widget	
			color: Color white;
			borderInset;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself.
]

{ #category : #'interface building' }
FontChooserMorph >> newPointSizeList [ 

	| widget |
	widget := PluggableListMorph
		on: self model
		list: #pointSizeList
		selected: #selectedPointSizeIndex
		changeSelected: #selectedPointSizeIndex:.
	^ widget	
			color: Color white;
			borderInset;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself.
]

{ #category : #'ui elements' }
FontChooserMorph >> okButton [

	okButton ifNotNil: [ ^ okButton ].
	okButton := self
			newButtonFor: self
			action: #okButtonClicked
			label: 'OK' translated
			help: 'Click here to close this dialog, and accept your selection' translated.
		^ okButton
			hResizing: #spaceFill;
			yourself.
]

{ #category : #actions }
FontChooserMorph >> okButtonClicked [

	self apply.
	self delete.
]

{ #category : #'open/close' }
FontChooserMorph >> open [

	^ self createWindow openAsIsIn: self currentWorld.
]

{ #category : #'interface building' }
FontChooserMorph >> pangrams [
	^ OrderedCollection new
		add: 'English' -> 'the quick brown fox jumps over the lazy dog';
		add: 'Bulgarian' -> 'Под южно дърво, цъфтящо в синьо, бягаше малко пухкаво зайче.';
		add: 'Czech' -> 'Příliš žluťoučký kůň úpěl ďábelské ódy.';
		add: 'Chinese' -> '視野無限廣，窗外有藍天
    微風迎客，軟語伴茶';
		add: 'Danish' -> 'Quizdeltagerne spiste jordbær med fløde, mens cirkusklovnen Walther spillede på xylofon.';
		add: 'Dutch' -> 'Pa''s wijze lynx bezag vroom het fikse aquaduct';
		add: 'Esperanto' -> 'Eĥoŝanĝo ĉiuĵaŭde';
		add: 'Estonian' -> 'See väike mölder jõuab rongile hüpata';
		add: 'Finnish' -> 'Viekas kettu punaturkki laiskan koiran takaa kurkki';
		add: 'French' -> 'Voix ambiguë d''un cœur qui au zéphyr préfère les jattes de kiwis';
		add: 'German' -> 'Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich';
		add: 'Greek' -> 'Θέλει αρετή και τόλμη η ελευθερία. (Ανδρέας Κάλβος)';
		add: 'Hebrew' -> 'דג סקרן שט לו בים זך אך לפתע פגש חבורה נחמדה שצצה כך';
		add: 'Hungarian' -> 'Egy hűtlen vejét fülöncsípő, dühös mexikói úr Wesselényinél mázol Quitóban';
		add: 'Italian' -> 'Ma la volpe, col suo balzo, ha raggiunto il quieto Fido.';
		add: 'Japanese' -> 'いろはにほへと ちりぬるを わかよたれそ つねならむ うゐのおくやま けふこえて あさきゆめみし ゑひもせす';
		add: 'Korean' -> '다람쥐 헌 쳇바퀴에 타고파';
		add: 'Latvian' -> 'Sarkanās jūrascūciņas peld pa jūru.';
		add: 'Norwegian' -> 'En god stil må først og fremst være klar. Den må være passende. Aristoteles.';
		add: 'Portugese' -> 'A rápida raposa castanha salta por cima do cão lento.';
		add: 'Brazilian portugese' -> 'A ligeira raposa marrom ataca o cão preguiçoso';
		add: 'Polish' -> 'Pchnąć w tę łódź jeża lub ośm skrzyń fig';
		add: 'Romanian' -> 'Agera vulpe maronie sare peste câinele cel leneş';
		add: 'Russian' -> 'Съешь ещё этих мягких французских булок да выпей же чаю';
		add: 'Serbian (cyrillic)' -> 'Чешће цeђење мрeжастим џаком побољшава фертилизацију генских хибрида.';
		add: 'Serbian (latin)' -> 'Češće ceđenje mrežastim džakom poboljšava fertilizaciju genskih hibrida.';
		add: 'Slovak' -> 'Kŕdeľ šťastných ďatľov učí pri ústí Váhu mĺkveho koňa obhrýzať kôru a žrať čerstvé mäso.';
		add: 'Slovene' -> 'V kožuščku hudobnega fanta stopiclja mizar in kliče 0619872345.';
		add:
				'Spanish'
						->
							'El veloz murciélago hindú comía feliz cardillo y kiwi. La cigüeña tocaba el saxofón detrás del palenque de paja.';
		add: 'Swedish' -> 'Flygande bäckasiner söka hwila på mjuka tuvor';
		add:
				'Thai'
						->
							'เป็นมนุษย์สุดประเสริฐเลิศคุณค่า
    กว่าบรรดาฝูงสัตว์เดรัจฉาน
    จงฝ่าฟันพัฒนาวิชาการ อย่าล้างผลาญฤๅเข่นฆ่าบีฑาใคร
    ไม่ถือโทษโกรธแช่งซัดฮึดฮัดด่า
    หัดอภัยเหมือนกีฬาอัชฌาสัย
    ปฏิบัติประพฤติกฎกำหนดใจ
    พูดจาให้จ๊ะ ๆ จ๋า ๆ น่าฟังเอยฯ';
		add: 'Turkish' -> 'Pijamalı hasta, yağız şoföre çabucak güvendi';
		yourself
]

{ #category : #accessing }
FontChooserMorph >> pointSizeString [

	^ model pointSize asString.
]

{ #category : #actions }
FontChooserMorph >> pointSizeString: aText [
	
	| string number |
	string := aText asString trimBoth.
	string isEmpty ifTrue: [^self].
	string
		detect: [:c | c isDigit not and: [c ~= $.]]
		ifFound: [^self].
	[number := string asNumber asFloat] on: Error do: [:e | ^self].
	(number < 1 or: [number > 1024])
		ifTrue: [^self].

	pointSizeMorph ifNotNil: [pointSizeMorph hasUnacceptedEdits: false].
	model pointSize: number
]

{ #category : #accessing }
FontChooserMorph >> previewText [

	"Answer the preview text based on current font."

	| sample i maxLineLength endOfLineCharacters |

	model selectedFont ifNil: [ ^ '' ].
	sample := String new writeStream.
	model selectedFont isSymbolFont
		ifFalse: [ | pangram |

			pangram := self selectedPangram.
			sample
				nextPutAll: pangram;
				cr;
				nextPutAll: pangram asUppercase;
				cr
			].
	i := 0.
	maxLineLength := 30.
	endOfLineCharacters := '@Z`z'.
	33 to: 255 do: [ :asciiValue | 
		| character |

		character := Character value: asciiValue.
		sample nextPut: character.
		i := i + 1.
		( ( endOfLineCharacters includes: character ) or: [ i = maxLineLength ] )
			ifTrue: [ i := 0.
				sample cr
				]
		].
	^ sample contents
]

{ #category : #accessing }
FontChooserMorph >> result [
	^ result
]

{ #category : #accessing }
FontChooserMorph >> selectedPangram [
	^ selectedPangram ifNil: [ self pangrams first value ]
]

{ #category : #'ui elements' }
FontChooserMorph >> updateButton [

	updateButton ifNotNil: [ ^ updateButton ].
	updateButton := self
		newButtonFor: self
		action: #updateButtonClicked
		label: 'Update' translated
		help: 'Click here to rescan Font Folder and update the font list' translated.
	^ updateButton
		hResizing: #spaceFill;
		yourself.
]

{ #category : #actions }
FontChooserMorph >> updateButtonClicked [

	self updateFontList.
]

{ #category : #actions }
FontChooserMorph >> updateFontIndex [

	fontList ifNotNil: [ fontList selectionIndex: model selectedFontIndex ].
	styleList ifNotNil: [ styleList updateList ].
	pointSizeList ifNotNil: [ pointSizeList updateList ].
	self updatePreview
]

{ #category : #actions }
FontChooserMorph >> updateFontList [

	model updateFontList.
	self announcer announce: #fontsUpdated.
]

{ #category : #actions }
FontChooserMorph >> updatePointSize [

	pointSizeList ifNotNil: [ pointSizeList selectionIndex: model selectedPointSizeIndex ].
	"this should not be like that but managed by the list!!!!"
	
	self updatePreview
]

{ #category : #actions }
FontChooserMorph >> updatePointSizeList [

	pointSizeList ifNotNil: [ pointSizeList updateList ].
	self updatePreview
]

{ #category : #actions }
FontChooserMorph >> updatePreview [

	 self fontPreviewPanel scrollTarget: self newFontPreviewInnerPanel
]

{ #category : #actions }
FontChooserMorph >> updateStyleList [

	styleList ifNotNil: [ styleList selectionIndex: model selectedFontStyleIndex ].
	"this should not be like that but managed by the list!!!!"
	
	self updatePreview
]

{ #category : #private }
FontChooserMorph >> widthRequiredFor: aBlock from: aCollection noLessThan: anInteger [
	"aBlock - takes each item as an argument, and returns the width it needs"

	^ aCollection inject: anInteger into: [ :max :each | | itemWidth |
		itemWidth := aBlock value: each.
		max max: itemWidth ].
]
