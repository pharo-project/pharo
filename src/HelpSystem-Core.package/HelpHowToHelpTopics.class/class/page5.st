pages
page5
	^HelpTopic 
		title: '5. Help sources'
		contents:	
	'Since the underlying model is very simple you can easily fill it with nearly any information from different sources. Try this:

|topic day url sub|
topic := HelpTopic named: ''Last week on Squeak IRC''.
0 to: 7 do: [:index |
	day := (Date today subtractDays: index) printFormat: #(3 2 1 $. 1 2 2).
	url := ''http://tunes.org/~nef/logs/squeak/'' , day.
	sub := HelpTopic 
			title: day contents: (ZnEasy get: url) contents.
	topic addSubtopic: sub.
].
HelpBrowser openOn: topic

 
 

' 