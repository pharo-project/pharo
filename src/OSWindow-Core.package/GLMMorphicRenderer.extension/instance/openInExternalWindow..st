*OSWindow-Core
openInExternalWindow: aRenderable
    window := GLMOSWindowMorphWorldRenderer render: aRenderable from: self.
    ^ window open