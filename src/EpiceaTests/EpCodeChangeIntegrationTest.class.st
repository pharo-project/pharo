Class {
	#name : #EpCodeChangeIntegrationTest,
	#superclass : #EpEnabledIntegrationTest,
	#instVars : [
		'aClass'
	],
	#category : #'EpiceaTests-Integration'
}

{ #category : #tests }
EpCodeChangeIntegrationTest >> categoryNameForTesting [

	^ #'EpiceaTestingWith-A-Really-WeirdCategoryName'

]

{ #category : #running }
EpCodeChangeIntegrationTest >> tearDown [
	super tearDown.
	
	self class environment organization removeCategory: self categoryNameForTesting
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testBehaviorCommentChange [

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpBehaviorCommentChange) equals: 0.
	
	aClass classComment: 'hey!' stamp: DateAndTime now.

	self assert: (self countLogEventsWith: EpBehaviorCommentChange) equals: 1.
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testCategoryAddition [

	self assert: (self countLogEventsWith: EpCategoryAddition) equals: 0.

	self class environment organization addCategory: self categoryNameForTesting.
	
	self assert: (self countLogEventsWith: EpCategoryAddition) equals: 1.
	self assert: (self allLogEventsWith: EpCategoryAddition) first affectedPackageName equals: self categoryNameForTesting
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testCategoryRemoval [

	self class environment organization addCategory: self categoryNameForTesting.

	self assert: (self countLogEventsWith: EpCategoryRemoval) equals: 0.

	self class environment organization removeCategory: self categoryNameForTesting.
	
	self assert: (self countLogEventsWith: EpCategoryRemoval) equals: 1.
	self assert: (self allLogEventsWith: EpCategoryRemoval) first affectedPackageName equals: self categoryNameForTesting
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testClassAddition [

	self assert: (self countLogEventsWith: EpClassAddition) equals: 0.

	classFactory newClass.
	
	self assert: (self countLogEventsWith: EpClassAddition) equals: 1.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testClassInstanceVariableAddition [

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
		
	aClass class addInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasMetaclassChanges
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testClassInstanceVariableRemoval [

	aClass := classFactory newClass.
	aClass class addInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
		
	aClass class removeInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 2.
	self assert: ((self allLogEventsWith: EpClassModification) allSatisfy: #hasMetaclassChanges)
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testClassModificationOfTraitComposition [
	<expectedFailure>
	"Failure: Currently, there is no system announcement when the trait composition is changed. 
	Epicea, confussingly, only registers method addition and modification events 
	when the trait is installed."
	
	| aTrait |
	aClass := classFactory newClass.
	aTrait := classFactory newTrait.
	aClass setTraitComposition: aTrait asTraitComposition.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self countLogEventsWith: EpMethodAddition) equals: 0.
	self assert: (self countLogEventsWith: EpMethodModification) equals: 0.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testClassRemoval [

	| className events |
	aClass := classFactory newClass.
	className := aClass name.
	aClass removeFromSystem.
	
	events := self allLogEventsWith: EpClassRemoval.
	self assert: events size equals: 1.
	"It shouldn't begin with AnObsolete"
	self assert: events anyOne behaviorAffectedName equals: className.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testInstanceVariableAddition [

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
		
	aClass addInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasNonMetaclassChanges
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testInstanceVariableRemoval [

	aClass := classFactory 
		newSubclassOf: Object 
		instanceVariableNames: 'x' 
		classVariableNames: ''.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	
	aClass removeInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasNonMetaclassChanges
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testMetaclassInstanceVariableAddition [

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	aClass theMetaClass instanceVariableNames: 'x'.	
	self assert: (self countLogEventsWith: EpClassModification) equals: 1.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testMetaclassInstanceVariableAdditionWithTrait [

	| aTrait |
	aClass := classFactory newClass.
	aTrait := classFactory newTrait.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	aClass theMetaClass uses: aTrait asTraitComposition instanceVariableNames: 'x'.	
	self assert: (self countLogEventsWith: EpClassModification) equals: 1.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testMethodAddition [

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.

	self assert: (self countLogEventsWith: EpMethodAddition) equals: 1
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testMethodModificationOfProtocol [

	| events event |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'old'.
	(aClass>>#fortyTwo) protocol: 'new'.

	events := self allLogEventsWith: EpMethodModification.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event oldMethod protocol equals: 'old'.
	self assert: event newMethod protocol equals: 'new'.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testMethodModificationOfSourceCode [

	| events event |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	aClass compile: 'fortyTwo ^nil'.

	events := self allLogEventsWith: EpMethodModification.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event oldMethod sourceCode equals: 'fortyTwo ^42'.
	self assert: event newMethod sourceCode equals: 'fortyTwo ^nil'.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testMethodModificationWithWideString [

	aClass := classFactory newClass.
	aClass compile: self wideStringSourceCode.
	aClass compile: self wideStringSourceCode, ' '.

	self assert: (self countLogEventsWith: EpMethodModification) equals: 1
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testMethodProtocolChange [

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: #accessing.
	aClass compile: 'fortyTwo ^42' classified: #testing.

	self assert: (self countLogEventsWith: EpMethodModification) equals: 2
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testMethodRecompilationShouldNotLog [

	| headBeforeRecompiling |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.

	headBeforeRecompiling := monitor log head.

	aClass compile: 'fortyTwo ^42'.
	
	self assert: monitor log head == headBeforeRecompiling
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testProtocolAddition [

	aClass := classFactory newClass.
	aClass organization addCategory: #testing42.

	self assert: (self countLogEventsWith: EpProtocolAddition) equals: 1
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testProtocolRemoval [

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: #testing42.
	aClass removeProtocol: #testing42.

	self assert: (self countLogEventsWith: EpProtocolRemoval) equals: 1
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testProtocolRemovalOfUnclassifiedProtocol [
	"This protocol is not relevant then it's not logged."

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: Protocol unclassified.
	aClass removeProtocol: Protocol unclassified.

	self assert: (self countLogEventsWith: EpProtocolRemoval) equals: 0
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testRedundantClassDefinition [

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
		
	self class compiler evaluate: aClass definition.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.

	self class compiler evaluate: aClass theMetaClass definition.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testRemoveTraitFromComposition [

	aClass := classFactory newClassUsing: TSizeTest.

	self 
		shouldnt: [ 
			aClass superclass 
				subclass: aClass name 
				uses: {} asTraitComposition 
				instanceVariableNames: ''
				classVariableNames: ''
				category: aClass category. ] 
		raise: MessageNotUnderstood
	
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testTraitAddition [

	| trait category events event |
	trait := classFactory newTrait.
	category := trait category.
	
	events := self allLogEventsWith: EpTraitAddition.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event behaviorAffected category equals: category.
]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testTraitModificationOfTraitComposition [
	<expectedFailure>
	"Failure: Currently, there is no system announcement when the trait composition is changed. 
	Epicea, confussingly, only registers method addition and modification events 
	when the trait is installed."
	
	| aTrait |
	aClass := classFactory newTrait.
	aTrait := classFactory newTrait.
	aClass setTraitComposition: aTrait asTraitComposition.

	self assert: (self countLogEventsWith: EpTraitModification) equals: 1.
	self assert: (self countLogEventsWith: EpMethodAddition) equals: 0.
	self assert: (self countLogEventsWith: EpMethodModification) equals: 0.

]

{ #category : #tests }
EpCodeChangeIntegrationTest >> testTraitRemoval [

	| trait category events event |
	trait := classFactory newTrait.
	category := trait category.
	trait removeFromSystem.
	
	events := self allLogEventsWith: EpTraitRemoval.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event behaviorAffected category equals: category.
]

{ #category : #running }
EpCodeChangeIntegrationTest >> wideStringSourceCode [
	
	^ 
'pangrams
	^ OrderedCollection new
		add: ''English'' -> ''the quick brown fox jumps over the lazy dog'';
		add: ''Bulgarian'' -> ''Под южно дърво, цъфтящо в синьо, бягаше малко пухкаво зайче.'';
		add: ''Czech'' -> ''Příliš žluťoučký kůň úpěl ďábelské ódy.'';
		add: ''Chinese'' -> ''視野無限廣，窗外有藍天
    微風迎客，軟語伴茶'';
		add: ''Danish'' -> ''Quizdeltagerne spiste jordbær med fløde, mens cirkusklovnen Walther spillede på xylofon.'';
		add: ''Dutch'' -> ''Pa''''s wijze lynx bezag vroom het fikse aquaduct'';
		add: ''Esperanto'' -> ''Eĥoŝanĝo ĉiuĵaŭde'';
		add: ''Estonian'' -> ''See väike mölder jõuab rongile hüpata'';
		add: ''Finnish'' -> ''Viekas kettu punaturkki laiskan koiran takaa kurkki'';
		add: ''French'' -> ''Voix ambiguë d''''un cœur qui au zéphyr préfère les jattes de kiwis'';
		add: ''German'' -> ''Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich'';
		add: ''Greek'' -> ''Θέλει αρετή και τόλμη η ελευθερία. (Ανδρέας Κάλβος)'';
		add: ''Hebrew'' -> ''דג סקרן שט לו בים זך אך לפתע פגש חבורה נחמדה שצצה כך'';
		add: ''Hungarian'' -> ''Egy hűtlen vejét fülöncsípő, dühös mexikói úr Wesselényinél mázol Quitóban'';
		add: ''Italian'' -> ''Ma la volpe, col suo balzo, ha raggiunto il quieto Fido.'';
		add: ''Japanese'' -> ''いろはにほへと ちりぬるを わかよたれそ つねならむ うゐのおくやま けふこえて あさきゆめみし ゑひもせす'';
		add: ''Korean'' -> ''다람쥐 헌 쳇바퀴에 타고파'';
		add: ''Latvian'' -> ''Sarkanās jūrascūciņas peld pa jūru.'';
		add: ''Norwegian'' -> ''En god stil må først og fremst være klar. Den må være passende. Aristoteles.'';
		add: ''Portugese'' -> ''A rápida raposa castanha salta por cima do cão lento.'';
		add: ''Brazilian portugese'' -> ''A ligeira raposa marrom ataca o cão preguiçoso'';
		add: ''Polish'' -> ''Pchnąć w tę łódź jeża lub ośm skrzyń fig'';
		add: ''Romanian'' -> ''Agera vulpe maronie sare peste câinele cel leneş'';
		add: ''Russian'' -> ''Съешь ещё этих мягких французских булок да выпей же чаю'';
		add: ''Serbian (cyrillic)'' -> ''Чешће цeђење мрeжастим џаком побољшава фертилизацију генских хибрида.'';
		add: ''Serbian (latin)'' -> ''Češće ceđenje mrežastim džakom poboljšava fertilizaciju genskih hibrida.'';
		add: ''Slovak'' -> ''Kŕdeľ šťastných ďatľov učí pri ústí Váhu mĺkveho koňa obhrýzať kôru a žrať čerstvé mäso.'';
		add: ''Slovene'' -> ''V kožuščku hudobnega fanta stopiclja mizar in kliče 0619872345.'';
		add:
				''Spanish''
						->
							''El veloz murciélago hindú comía feliz cardillo y kiwi. La cigüeña tocaba el saxofón detrás del palenque de paja.'';
		add: ''Swedish'' -> ''Flygande bäckasiner söka hwila på mjuka tuvor'';
		add:
				''Thai''
						->
							''เป็นมนุษย์สุดประเสริฐเลิศคุณค่า
    กว่าบรรดาฝูงสัตว์เดรัจฉาน
    จงฝ่าฟันพัฒนาวิชาการ อย่าล้างผลาญฤๅเข่นฆ่าบีฑาใคร
    ไม่ถือโทษโกรธแช่งซัดฮึดฮัดด่า
    หัดอภัยเหมือนกีฬาอัชฌาสัย
    ปฏิบัติประพฤติกฎกำหนดใจ
    พูดจาให้จ๊ะ ๆ จ๋า ๆ น่าฟังเอยฯ'';
		add: ''Turkish'' -> ''Pijamalı hasta, yağız şoföre çabucak güvendi'';
		yourself'
]
