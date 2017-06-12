ClassTestCase removeSelector: #testClassComment.
HashTesterTest removeSelector: #testBasicBehaviour.


(RPackage organizer packages collect: #name) logCr.

HDTestReport runPackages: (RPackage organizer packages collect: #name).

"HDTestReport runClasses: Smalltalk allClasses named: 'Tests'."

Smalltalk exitSuccess. 