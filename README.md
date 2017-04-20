# sharesafe-client

This will be the client's running proxy.

In order to reduce the risk of leaking user's secret keys, we will require
users to install this application.

It will act as a proxy. Managing:

* the communication toward the main registry;
* the local keys and secrets;
* user's favorites and history (so they can retrieved used keys or users)
* the user interface

# About the sources

## library `sharesafe-client`

This will define all the necessary information for both the user interface,
the common details about front-end and backend.

It contains mainly the type definitions that are going to be used for both
the local server `sharesafe` and the `purescript` user interface.

We will also provide the API definition for the local server `sharesafe`
in order to generate javascript to call the function as well.

## executable `sharesafe-client-purescript-bridge`

This will generate the purescript compatible types in the `ui` directory.

## executable `sharesafe`

The application running the different operations for the user. Providing a
local servant API (for the `purescript` user interface) and forwarding the
calls to the sharesafe server.
