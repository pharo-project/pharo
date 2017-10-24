publishing
generateSystemReport

	<script>
	
	| report |
	
	report := DADependencyChecker new computeImageDependencies.
'dependency-report.html' asFileReference writeStreamDo: [ :stream |
	 DADependenciesHTMLPublisher 
		publishReportFrom: report
		stream: stream ].

	