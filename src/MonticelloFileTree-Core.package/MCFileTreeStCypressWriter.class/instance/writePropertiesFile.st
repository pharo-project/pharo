initialize-release
writePropertiesFile
  | properties |
  properties := Dictionary new.
  properties at: 'noMethodMetaData' put: true.
  properties at: 'separateMethodMetaAndSource' put: false.
  properties at: 'useCypressPropertiesFile' put: true.
  self
    writeInDirectoryName: '.'
    fileName: ''
    extension: '.filetree'
    visit: [ STON put: properties asJsonOnStreamPretty: fileStream ].
  self
    writeInDirectoryName: '.'
    fileName: 'properties'
    extension: self propertyFileExtension
    visit: [ STON put: (Dictionary new) asJsonOnStreamPretty: fileStream ]