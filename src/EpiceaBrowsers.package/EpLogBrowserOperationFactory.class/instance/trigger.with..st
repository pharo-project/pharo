private
trigger: aBlock with: anEpLogBrowserOperation

	^ anEpLogBrowserOperation doInJob: aBlock