#lang sicp
;; chapter 1: primitive data and operations
;;            combine procedures
;;              composition, conditions, parameters, define
;;            high-order procedures
;;              manipulation, general computation
;; essence of programming

;; chapter 2: complex data -- combine data objects to form compound data
;; enhance our expressive power!
;; reduce bookkeeping

;; data abstraction: separate the definition of compound data from their operations
;; makes programs easier to design, maintain and modify

;; implement a rational-number arithmetic system , illustrates compound data and abstraction
;; possible to "glue" data objects together using only procedures

;; key idea: closure, glue should work for both primitive and compound data objects
;; conventional interfaces - combine program modules in mix-and-match ways 