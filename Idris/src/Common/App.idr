module Common.App

import Control.App


||| Environment for App context
public export
Environment : Type
Environment = List Type

public export
interface Validation e where
  validation : String -> App {l} e Bool
