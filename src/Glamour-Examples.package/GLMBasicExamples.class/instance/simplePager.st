browsers
simplePager
	<glmBrowser: 'Pager' input: '42'>
	"self new simplePager openOn: 42"
	| pager |	
	pager := GLMPager new.
	pager show: [:a | a list display: [ :x | 1 to: x ]].
	^ pager