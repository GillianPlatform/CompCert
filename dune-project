(lang dune 2.9)
(using dune_site 0.1)
(name compcert)


(authors "Sacha Ayoun, CompCert Team")
(maintainers "Sacha Ayoun")


(package
 (name compcert)
 (synopsis "CompCert packaged for Gillian")
 (sites (share iniConfig))
 (depends
  menhir
  dune-site))