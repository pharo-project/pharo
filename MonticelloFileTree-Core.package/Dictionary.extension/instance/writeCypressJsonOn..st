*monticellofiletree-core
writeCypressJsonOn: aStream
    self writeCypressJsonOn: aStream forHtml: false indent: 0.
    aStream lf