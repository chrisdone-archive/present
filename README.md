present
=====

Make presentations for data types.

## Requirements

The following language extensions are required to be enabled in your
GHC session:

    -XTemplateHaskell -XScopedTypeVariables -XOverloadedStrings

## Usage

In a file that you're going to load into GHCi, put `$(makePresent 'x)`
for the thing you want to present.
