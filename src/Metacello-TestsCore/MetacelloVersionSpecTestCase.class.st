Class {
	#name : #MetacelloVersionSpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsCore-Specs'
}

{ #category : #tests }
MetacelloVersionSpecTestCase >> testVersionMergeSpec [
    | versionA versionB version |
    versionA := self versionSpec
        blessing: #'baseline';
        versionString: '1.0';
        description: 'A description';
        author: 'dkh';
        timestamp: '1/24/2012 09:59';
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    versionB := self versionSpec
        blessing: #'release';
        versionString: '1.1';
        description: 'A FULL description';
        author: 'DaleHenrichs';
        timestamp: '1/24/2012 10:22';
        preLoadDoIt: #'preLoadDoItB';
        postLoadDoIt: #'postLoadDoItB';
        yourself.
    version := versionA mergeSpec: versionB.
    self assert: version blessing value = #'release'.
    self assert: version versionString value = '1.1'.
    self assert: version description value = 'A FULL description'.
    self assert: version author value = 'DaleHenrichs'.
    self assert: version timestamp value = '1/24/2012 10:22'.
    self assert: version preLoadDoIt value == #'preLoadDoItB'.
    self assert: version postLoadDoIt value == #'postLoadDoItB'
]

{ #category : #tests }
MetacelloVersionSpecTestCase >> testVersionSpec [
    | version |
    version := self versionSpec
        blessing: #'baseline';
        versionString: '1.0';
        description: 'A description';
        author: 'dkh';
        timestamp: '1/24/2012 09:59';
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    self assert: version blessing value = #'baseline'.
    self assert: version versionString value = '1.0'.
    self assert: version description value = 'A description'.
    self assert: version author value = 'dkh'.
    self assert: version timestamp value = '1/24/2012 09:59'.
    self assert: version preLoadDoIt value == #'preLoadDoIt'.
    self assert: version postLoadDoIt value == #'postLoadDoIt'.
    self should: [ version preLoadDoIt: '' ] raise: Error.
    self should: [ version postLoadDoIt: '' ] raise: Error
]

{ #category : #tests }
MetacelloVersionSpecTestCase >> testVersionSpecCreateVersion [
    | spec version |
    spec := self versionSpec
        blessing: #'baseline';
        versionString: '1.0';
        description: 'A description';
        author: 'dkh';
        timestamp: '1/24/2012 09:59';
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    version := spec createVersion.
    self assert: version class == spec versionClass.
    self assert: version spec == spec.
    self assert: version versionNumber asString = '1.0'.
    self assert: version blessing = #'baseline'.
    self assert: version versionString = '1.0'.
    self assert: version description = 'A description'.
    self assert: version author = 'dkh'.
    self assert: version timestamp = '1/24/2012 09:59'
]
