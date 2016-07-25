text
textPortAction
	<glmBrowser: 'Text port action' input: '''Type some text here'''>
	"self new textPortAction openOn: 'Type some text here'"
	
	| browser morph |
   morph := StringMorph new.
   morph font:(LogicalFont familyName: 'Source Code Pro' pointSize: 30).
   browser := GLMTabulator new.
   browser
   		row: #main;
		row: #preview.
   browser transmit
      to: #main;
      andShow: [ :a | a text
        onChangeOfPort: #text act: [ :text  |
            morph contents:text text] ].
   browser transmit
		to: #preview;
		andShow: [ :a | a morph
			title: 'Preview';
          display: [ morph ] ].

 	^ browser