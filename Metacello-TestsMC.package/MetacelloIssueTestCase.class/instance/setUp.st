running
setUp
  | repo |
  super setUp.
  MetacelloPlatform current clearPackageCache.
  repo := self monticelloRepository.
  gofer := Gofer new.
  gofer disablePackageCache.
  gofer repository: repo.
  Smalltalk at: #'Metacello_Gofer_Test_Repository' put: repo.
  repo := self configurationRepository.
  gofer repository: repo.
  Smalltalk at: #'Metacello_Configuration_Test_Repository' put: repo.
  initialWorkingCopyList := MCWorkingCopy allManagers
    collect: [ :each | each packageName ]