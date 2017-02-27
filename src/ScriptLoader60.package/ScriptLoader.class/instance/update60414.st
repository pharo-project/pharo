pharo - updates
update60414
	"self new update60414"
	self withUpdateLog: '19759 after UFFI update, SDL2 has unbound global class ExternalForm
	https://pharo.fogbugz.com/f/cases/19759

19760 MorphTreeNodeMorph>>canExpand should accept the nil value of complexContents
	https://pharo.fogbugz.com/f/cases/19760

19742 Some system baselines needs update
	https://pharo.fogbugz.com/f/cases/19742

19402 NNavNavigateTest should be in separate package
	https://pharo.fogbugz.com/f/cases/19402'.
	self loadTogether: self script60414 merge: false.
	self flushCaches.
