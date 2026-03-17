open System

type 't btree =
    Node of 't * 't btree * 't btree
    | Nil