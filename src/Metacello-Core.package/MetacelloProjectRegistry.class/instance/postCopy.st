copying
postCopy
    super postCopy.
    baselineRegistry := self baselineRegistry copy.
    configurationRegistry := self configurationRegistry copy