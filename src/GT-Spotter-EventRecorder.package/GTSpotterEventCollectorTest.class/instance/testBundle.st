tests
testBundle
	| events bundle |
	"collecting data"
	events := OrderedCollection new.
	events 
		add: GTSpotterRecorderOpenEvent new;
		add: GTSpotterRecorderShowPreviewEvent new;
		add: GTSpotterRecorderExitAnnouncementEvent new;
		add: GTSpotterRecorderOpenEvent new;
		add: GTSpotterRecorderShowPreviewEvent new;
		add: GTSpotterRecorderExitAnnouncementEvent new;
		add: GTSpotterRecorderOpenEvent new;
		add: GTSpotterRecorderShowPreviewEvent new.
	events do: [ :each | collector add: each ].
	"first delivery"
	bundle := collector bundle.
	self assert: collector postponedEvents size equals: 2.
	7 to: 8 do: [ :index |
		self assert: (events at: index) == (collector postponedEvents at: index - 6) ].
	self assert: bundle size equals: 6.
	bundle withIndexDo: [ :eachEvent :index |
		self assert: (events at: index) class = eachEvent class ].
	"second delivery"
	bundle := collector bundle.
	self assert: collector postponedEvents size equals: 2.
	7 to: 8 do: [ :index |
		self assert: (events at: index) == (collector postponedEvents at: index - 6) ].
	self assert: bundle isEmpty.
	"third delivery"
	collector add: GTSpotterRecorderExitAnnouncementEvent new.
	bundle := collector bundle.
	self assert: collector postponedEvents isNil.
	self assert: bundle size equals: 3.
	self assert: bundle first class equals: (events at: 7) class.
	self assert: bundle second class equals: (events at: 8) class.
	self assert: bundle third class equals: GTSpotterRecorderExitAnnouncementEvent.