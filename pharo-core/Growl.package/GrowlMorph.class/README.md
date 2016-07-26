A GrowlMorph is a little Morph to announce event happening. Freely inspired from the MIT Snarl developed by  Tony Garnock-Jones. 

GrowlMorph new openInWorld.

10 timesRepeat: [
	(GrowlMorph openWithLabel: 'The time' contents: DateAndTime now)
"		vanishDelay: 1000;
		resetVanishTimer".
	World doOneCycle ].

(GrowlMorph openWithLabel: 'The time' contents: DateAndTime now) 
	actionBlock: [Transcript open].