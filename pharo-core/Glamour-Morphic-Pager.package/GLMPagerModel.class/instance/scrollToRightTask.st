scrolling
scrollToRightTask

	^ scrollToRightTask ifNil: [
		scrollToRightTask := GLMAsyncTask new.
		scrollToRightTask
			doInBackground: [ :isSmooth | isSmooth ];
			onCompleted: [ :isSmooth | self scrollToRight: isSmooth ];
			delay: self scrollingDelay ]