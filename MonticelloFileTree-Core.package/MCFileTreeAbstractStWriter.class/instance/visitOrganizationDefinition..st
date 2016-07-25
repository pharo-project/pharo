visiting
visitOrganizationDefinition: defintion
    self
        writeInDirectoryName: self monticelloMetaDirName
        fileName: 'categories'
        extension: '.st'
        visit: [ defintion categories do: [ :cat | stWriter writeCategory: cat ] ]