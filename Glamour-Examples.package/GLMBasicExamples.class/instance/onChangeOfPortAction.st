actions examples
onChangeOfPortAction
	<glmBrowser: 'On change of port' input: '''Type something and check the Transcript'''>
	"self new onChangeOfPortAction openOn: 'Type something and check the Transcript' "

	^ GLMCompositePresentation new 
		with: [ :composite |
			composite text 
				onChangeOfPort: #text act: [ :textPresentation | 
					Transcript cr; cr; show: textPresentation text ] ];
		act: [ Transcript open ] entitled: 'Open Transcript'