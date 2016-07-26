private
collectorForInstancesVariable 
	^ collectorForInstancesVariable ifNil: [collectorForInstancesVariable := self createCollector: #instVarCommand].
