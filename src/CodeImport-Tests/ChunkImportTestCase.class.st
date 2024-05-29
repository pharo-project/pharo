"
SUnit test case for import of chunk format
"
Class {
	#name : 'ChunkImportTestCase',
	#superclass : 'TestCase',
	#category : 'CodeImport-Tests-Chunks',
	#package : 'CodeImport-Tests',
	#tag : 'Chunks'
}

{ #category : 'helpers' }
ChunkImportTestCase >> importAClass [

	^ CodeImporter evaluateString: 'Object subclass: #CodeImportTestCaseTestClass
			instanceVariableNames: ''var1 var2 var3''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: ''' , self packageNameForTests , '''!'
]

{ #category : 'helpers' }
ChunkImportTestCase >> packageNameForTests [

	^ #'Generated-CodeImport-Tests-Garbage'
]

{ #category : 'running' }
ChunkImportTestCase >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests.
	super tearDown
]

{ #category : 'tests' }
ChunkImportTestCase >> testExportAMethodWithLangTag [
	|  stream chunkWriteStream |
	stream := String new writeStream.
	chunkWriteStream := ChunkWriteStream on: stream.
	chunkWriteStream nextPut:'pangrams
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
		yourself'.

	self assert: stream contents equals: 'pangrams
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
		yourself!'
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportAClass [

	| class |
	class := self importAClass.
	self assert: class superclass equals: Object.
	self assert: #CodeImportTestCaseTestClass equals: class name.
	self assert: (class instVarNames includes: 'var1').
	self assert: (class instVarNames includes: 'var2').
	self assert: (class instVarNames includes: 'var3').
	self assert: class package name equals: 'Generated-CodeImport-Tests-Garbage'
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportAClassCategory [

	| class classOrganizationString chunk |
	classOrganizationString := '(#testing testImportAMethod testImportAClass)
(#auxiliar importAClass)'.
	chunk := '!{1} reorganize!'.

	class := self importAClass.

	"we write the methods we will categorize"
	class compileSilently: 'testImportAClass'.
	class compileSilently: 'testImportAMethod'.
	class compileSilently: 'importAClass'.

	CodeImporter evaluateString: (chunk format: { class name asString }) , classOrganizationString.
	"we do not care about the order of the output just that all the elements are present"
	self assert: (((String streamContents: [ :s | class fileOutProtocolsOn: s ]) findTokens: String cr , ' ()') includesAll:
			 (classOrganizationString trimBoth findTokens: String cr , ' ()'))
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportAClassComment [

	| class comment |
	comment := 'I''m a nice comment, treat me well :).'.

	class := self importAClass.
	CodeImporter evaluateString: ('!{1} commentStamp: ''<historical>'' prior: 0!{2}!' format: {
				 class name asString.
				 comment }).
	self assert: comment equals: class comment
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportAClassCommentWithExclamationMarks [

	| class comment commentToWrite |
	comment := 'I''m a nice comment!, treat me well :).'.
	commentToWrite := 'I''m a nice comment!!, treat me well :).' replaceAll: '!' with: '!!'.

	class := self importAClass.
	CodeImporter evaluateString: ('!{1} commentStamp: ''<historical>'' prior: 0!{2}!' format: {
				 class name asString.
				 commentToWrite }).
	self assert: comment equals: class comment
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportAMethod [

	| class |
	class := self importAClass.
	CodeImporter evaluateString: ('!{1} methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true' format: { class name asString }).
	self assert: class new someMethod
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportAMethodWithLangTag [

	| class |
	class := self importAClass.
	CodeImporter evaluateString: ('!{1} methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!pangrams
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
		yourself!]lang[(2798)0! !' format: { class name asString }).
	self assert: class new pangrams first value equals: 'the quick brown fox jumps over the lazy dog'
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportAMethodWithNoTimestamp [

	| class |
	class := self importAClass.
	CodeImporter evaluateString: ('!{1} methodsFor: ''some protocol''!someMethod
	^true' format: { class name asString }).
	self assert: class new someMethod
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportAMethodWithSpacesInItsCategory [

	| class |
	class := self importAClass.
	CodeImporter evaluateString: ('!{1} methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true' format: { class name asString }).

	self assert: (class >> #someMethod) protocolName equals: 'some protocol'
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportFromReadStream [
	self assert: 4 equals: (CodeImporter evaluateReadStream: '2+2!' readStream)
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportString [
	self assert: 4 equals: (CodeImporter evaluateString: '2+2!')
]

{ #category : 'tests' }
ChunkImportTestCase >> testImportUndeclared [
	self assert: 4 equals: (CodeImporter evaluateString: 'myCodeImortUndecl := 4')
]
