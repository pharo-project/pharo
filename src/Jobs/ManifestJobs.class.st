"
This package contains the code related to Jobs.

Jobs are a way to manage the run of a task and optionnaly make it easier to notify the user of the progress. They also announce the progress of the task (when it begins, progress and ends).

The main class to use is Job. Here is an example:

[:job | job title: 'Let us get started'.
	1 to: 10 do: [:each |
		job
			progress: (0.1 * each);
			title: 'Youpi ', each printString.
		100 milliSeconds wait.
		] ]  asJob run
"
Class {
	#name : #ManifestJobs,
	#superclass : #PackageManifest,
	#category : #'Jobs-Manifest'
}
