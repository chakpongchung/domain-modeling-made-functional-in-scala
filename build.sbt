ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "1.0.0"
ThisBuild / organization := "OrderTaking"
ThisBuild / organizationName := "OrderTaking"

lazy val root = (project in file("."))
  .settings(
    name := "domain-modeling-made-functional-in-scala"
  )
