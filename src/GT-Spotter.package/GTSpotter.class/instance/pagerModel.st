accessing
pagerModel

	^ pagerModel ifNil: [
		pagerModel := self defaultPagerModel defaultNumberOfVisiblePages: 1.
		pagerModel announcer when: GLMPagePoped send: #onPagePopped to: self.
		pagerModel ]