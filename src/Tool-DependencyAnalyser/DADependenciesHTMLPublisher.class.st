"
This class generates an HTML report of system dependencies in HTML format.

Example: 

 '/tmp/report.html' asFileReference writeStreamDo: [ :stream |
	 DADependenciesHTMLPublisher 
		publishReportFrom: (DADependencyChecker new transitiveDependenciesOf: #Kernel)
		stream: stream ]
"
Class {
	#name : #DADependenciesHTMLPublisher,
	#superclass : #Object,
	#instVars : [
		'stream',
		'report'
	],
	#category : #'Tool-DependencyAnalyser-HTMLReport'
}

{ #category : #publishing }
DADependenciesHTMLPublisher class >> generateSystemReport [

	<script>
	
	| report |
	
	report := DADependencyChecker new computeImageDependencies.
'dependency-report.html' asFileReference writeStreamDo: [ :stream |
	 DADependenciesHTMLPublisher 
		publishReportFrom: report
		stream: stream ].

	
]

{ #category : #publishing }
DADependenciesHTMLPublisher class >> publishReportFrom: aCGODependencyReport stream: aStream [ 
	self basicNew 
		initializeWithReport: aCGODependencyReport stream: aStream;
		publish
		
]

{ #category : #'html utilities' }
DADependenciesHTMLPublisher >> accordion: id name: name headingContent: heading body: body [
	stream << '<div class="panel-group" id="' << id << '" role="tablist" aria-multiselectable="true">'.
	(stream
		<<
		'  <div class="panel panel-default">
    <div class="panel-heading" role="tab" id="heading' << name
		<<
		'">
      <h4 class="panel-title">
        <a role="button" data-toggle="collapse" data-parent="#' << id << '" href="#collapse' << name
		<< '" aria-expanded="true" aria-controls="collapse' << name)
		<< '">';
		newLine.
	stream << heading
		<<
			'
        </a>
      </h4>
    </div>
    <div id="collapse' << name << '" class="panel-collapse collapse in" role="tabpanel" aria-labelledby="heading' << name
		<<
			'">
      <div class="panel-body">
      		'.
	stream << body
		<<
			'
      </div>
    </div>
  </div>'.
	stream
		<< '</div>';
		newLine
]

{ #category : #'html utilities' }
DADependenciesHTMLPublisher >> badgeFor: aString [
	^ '<span class="badge"> ' , aString , '</span>'
]

{ #category : #'html utilities' }
DADependenciesHTMLPublisher >> dangerLabel: aString [
	^ '<span class="label label-danger">' , aString , '</span>'
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> htmlForDependencies: dependencies of: aPackageName [
	^ String streamContents: [ :str |
		dependencies
			do: [ :dependency | str 
				<< '<a href="#'
				<< dependency
				<< '">'
				<< (self htmlForDependency: dependency of: aPackageName)
				<< '</a>' ]
			separatedBy: [ str space ] ]
		

]

{ #category : #publishing }
DADependenciesHTMLPublisher >> htmlForDependency: dependencyPackageName of: aPackageName [
	^ 	dependencyPackageName

]

{ #category : #publishing }
DADependenciesHTMLPublisher >> htmlForIgnoredDependenciesOf: aPackageName [
	^ String streamContents: [ :str |
		(RPackage organizer packageNamed: aPackageName) ignoredDependencies 
			do: [ :dependency | str << (self warningLabel: dependency) ]
			separatedBy: [ str space ] ]
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> htmlForPackage: aPackageName [
	^ 	aPackageName

]

{ #category : #publishing }
DADependenciesHTMLPublisher >> imageVersionString [
	^ report imageVersion majorMinorVersion, ' update ' , report imageVersion highestUpdate printString
]

{ #category : #initialization }
DADependenciesHTMLPublisher >> initializeWithReport: aCGODependencyReport stream: aStream [
	super initialize.
	stream := aStream.
	report := aCGODependencyReport
]

{ #category : #'html utilities' }
DADependenciesHTMLPublisher >> javaScript: scriptFileName [
	(stream << '<script type="text/javascript" language="javascript" src="' << scriptFileName)
		<< '"></script>';
		newLine
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publish [
	self 
		publishHTMLHeader;
		publishSetup;
		publishWarnings;
		publishDependenciesCaption;
		publishDependencies;
		publishDependenciesCaption;
		publishHTMLFooter.
	stream close.
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publish: aPackageName dependants: dependants [
	| dependantLinks |
	dependants ifEmpty: [ ^ self ].
	dependantLinks := dependants sorted collect: [ :name | '<a href="#' , name , '">' , name , '</a>' ].
	(stream
		<<
		'						<tr>
							<td id="' << aPackageName << '">' << (self htmlForPackage: aPackageName)
		<<
		'</td>
							<td> ' << (self badgeFor: 'N/A')
		<<
		' </td>
							<td>' << (self sizeBadgeFor: dependants) << Character space << (Character space join: dependantLinks))
		<<
			'</td>
						</tr>';
		newLine
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publish: aPackageName dependencies: dependencies dependants: dependants [
	| dependantLinks |
	dependantLinks := dependants sorted collect: [ :name | '<a href="#' , name , '">' , name , '</a>' ].
	(stream
		<<
		'						<tr>
							<td id="' << aPackageName << '">' << (self htmlForPackage: aPackageName)
		<<
		'</td>
							<td>' << (self sizeBadgeFor: dependencies) << Character space
		<< (self htmlForDependencies: dependencies sorted of: aPackageName) << Character space
		<< (self htmlForIgnoredDependenciesOf: aPackageName)
		<<
		'</td>
							<td>' << (self sizeBadgeFor: dependants) << Character space << (Character space join: dependantLinks))
		<<
			'</td>
						</tr>';
		newLine
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publishDependencies [
	stream
		<<
			'				<table id="example" class="display" cellspacing="0" width="100%">
					<thead>
						<tr>
							<th>Package</th>
							<th>Dependencies</th>
							<th>Dependants</th>
						</tr>
					</thead>
					<tbody>';
		newLine.
	report
		dependenciesAndKnownDependantsDo:
			[ :packageName :dependencies :dependants | self publish: packageName dependencies: dependencies dependants: dependants ].
	self publishPackagesToRemoveDependants.
	stream
		<<
			'					</tbody>
				</table>';
		newLine
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publishDependenciesCaption [
	self stream
		newLine;
		<< (self warningLabel: 'Ignored dependency');
		newLine
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publishHTMLFooter [
	self stream
		newLine;
		<< '</div>';
		newLine;
		<< '</body>';
		newLine;
		<< '</html>';
		newLine
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publishHTMLHeader [
	self stream
		<< '<!DOCTYPE html>';
		newLine;
		<< '<html lang="en">';
		newLine;
		<< '<head>';
		newLine;
		<< '<title>Dependencies Report: </title>';
		newLine.
	self styleSheet: 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css'.
	self styleSheet: 'https://cdn.datatables.net/1.10.8/css/jquery.dataTables.min.css'.
	self javaScript: 'https://code.jquery.com/jquery-1.11.3.min.js'.
	self javaScript: 'https://cdn.datatables.net/1.10.8/js/jquery.dataTables.min.js'.
	self javaScript: 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js'.
	stream
		<<
			'<script type="text/javascript" class="init">
$(document).ready(function() {
	$(''#example'').DataTable( {
		"paging": false,
		"aoColumns": [ null, { "bSearchable": false }, { "bSearchable": false } ]
	});
} );
	</script>'.
	self stream
		newLine;
		<< '</head>';
		newLine;
		<< '<body>';
		newLine;
		<< '<div class="container">';
		newLine;
		<< '<h1>Dependency analysis</h1>';
		newLine
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publishPackagesToRemoveDependants [
	"Standard dependency report do not care about packages to remove for the bootsrap.
	Nothing to do."
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publishSetup [
	| body |
	body := String streamContents: [ :str |
		str
			<< 'Image version: <span class="label label-primary">';
			<< self imageVersionString; 
			<< '</span></br> Analysis run on ';
			<< report analysisRunString ; 
			<< '</br>' ].
	self accordion: 'accordionSetup' name: 'Setup' headingContent: 'Analysis setup' body: body
]

{ #category : #publishing }
DADependenciesHTMLPublisher >> publishWarnings [
	| body |
	body := String
		streamContents: [ :str | (report warnings sorted: [ :a :b | a package < b package ]) do: [ :warning | str << warning messageText << '<br/>' ] ].
	self
		accordion: 'accordionWarnings'
		name: 'Warnings'
		headingContent: 'Warnings ' , (self sizeBadgeFor: report warnings)
		body: body.
	stream
		<< '</br>';
		newLine
]

{ #category : #'html utilities' }
DADependenciesHTMLPublisher >> sizeBadgeFor: aCollection [
	^ self badgeFor: aCollection size asString
]

{ #category : #accessing }
DADependenciesHTMLPublisher >> stream [
	^ stream
]

{ #category : #'html utilities' }
DADependenciesHTMLPublisher >> styleSheet: styleSheetUrl [
	(stream << '<link rel="stylesheet" type="text/css" href="' << styleSheetUrl)
		<< '">';
		newLine
]

{ #category : #'html utilities' }
DADependenciesHTMLPublisher >> warningLabel: aString [
	^ '<span class="label label-warning">' , aString , '</span>'
]
