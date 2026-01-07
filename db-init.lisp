;;;; db-init.lisp — Database initialization

(in-package :expert)

(defun init-db ()
  (sql-exec "DROP TABLE IF EXISTS facts;")
  (sql-exec "DROP TABLE IF EXISTS rules;")
  (sql-exec "CREATE TABLE facts (code TEXT PRIMARY KEY, label TEXT, category TEXT);")
  (sql-exec "CREATE TABLE rules (id TEXT PRIMARY KEY, conds TEXT, conclusion TEXT, action_done TEXT);")

  ;; FACTS - Regroupés par catégorie

  ;; Catégorie: Performance
  (sql-exec "INSERT INTO facts VALUES ('F1','Mon ordinateur est devenu très lent','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F7','Le processeur ou le disque est utilisé à plus de 80%','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F23','Les applications mettent longtemps à se lancer','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F24','La navigation sur internet est lente','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F25','La copie de fichiers est anormalement lente','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F26','Je ne peux pas utiliser plusieurs applications en même temps','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F27','Mon ordinateur se fige régulièrement quelques secondes','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F28','Le défilement des pages est saccadé','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F29','L''ordinateur met du temps à répondre à mes actions','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F30','La mémoire RAM est saturée','Performance');")
  (sql-exec "INSERT INTO facts VALUES ('F31','Le fichier d''échange (swap) est très sollicité','Performance');")

  ;; Catégorie: Démarrage
  (sql-exec "INSERT INTO facts VALUES ('F2','Mon PC ne s''allume pas du tout','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F3','L''écran reste noir au démarrage','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F6','Le PC se bloque sur l''écran de démarrage','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F32','Le démarrage de Windows est très long','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F33','Un message d''erreur apparaît au démarrage','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F34','Le PC redémarre en boucle sans cesse','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F35','Un écran bleu (BSOD) apparaît au démarrage','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F36','Le BIOS ne s''affiche pas','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F37','Le ventilateur tourne mais l''écran reste noir','Démarrage');")
  (sql-exec "INSERT INTO facts VALUES ('F38','Le clavier et la souris ne répondent pas au démarrage','Démarrage');")

  ;; Catégorie: Réseau
  (sql-exec "INSERT INTO facts VALUES ('F4','Mon PC ne se connecte pas au WiFi','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F19','La carte réseau est bien détectée par Windows','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F39','La connexion Ethernet par câble ne fonctionne pas','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F40','Le WiFi se déconnecte fréquemment','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F41','Ma connexion internet est lente','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F42','Certains sites web sont inaccessibles','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F43','Je ne peux pas accéder aux fichiers partagés sur le réseau','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F44','L''imprimante réseau n''est pas accessible','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F45','Mon VPN ne parvient pas à se connecter','Réseau');")
  (sql-exec "INSERT INTO facts VALUES ('F46','Mon adresse IP est invalide ou en 169.x.x.x','Réseau');")

  ;; Catégorie: Thermique
  (sql-exec "INSERT INTO facts VALUES ('F5','Mon ordinateur surchauffe ou le ventilateur est très bruyant','Thermique');")
  (sql-exec "INSERT INTO facts VALUES ('F11','La température du processeur est élevée','Thermique');")
  (sql-exec "INSERT INTO facts VALUES ('F12','Les grilles d''aération sont encrassées ou obstruées','Thermique');")
  (sql-exec "INSERT INTO facts VALUES ('F47','Mon PC s''éteint tout seul sans prévenir','Thermique');")
  (sql-exec "INSERT INTO facts VALUES ('F48','Le ventilateur ne tourne pas','Thermique');")
  (sql-exec "INSERT INTO facts VALUES ('F49','L''ordinateur est très chaud au toucher','Thermique');")
  (sql-exec "INSERT INTO facts VALUES ('F50','Les performances baissent automatiquement (throttling)','Thermique');")
  (sql-exec "INSERT INTO facts VALUES ('F51','Le ventilateur fait un bruit irrégulier ou inhabituel','Thermique');")

  ;; Catégorie: Sécurité
  (sql-exec "INSERT INTO facts VALUES ('F8','Plusieurs antivirus sont installés sur mon PC','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F9','Un logiciel malveillant a été détecté','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F20','Un antivirus tiers (autre que Windows Defender) est installé','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F52','Des pop-ups publicitaires apparaissent sans arrêt','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F53','Mon navigateur me redirige vers des sites inconnus','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F54','Mes fichiers ont été cryptés par un ransomware','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F55','Mon mot de passe a été changé sans mon autorisation','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F56','Des logiciels inconnus se sont installés tout seuls','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F57','Le pare-feu Windows est désactivé','Sécurité');")
  (sql-exec "INSERT INTO facts VALUES ('F58','Les mises à jour de sécurité Windows ne sont pas installées','Sécurité');")

  ;; Catégorie: Disque
  (sql-exec "INSERT INTO facts VALUES ('F10','Le disque dur montre des signes de défaillance (S.M.A.R.T.)','Disque');")
  (sql-exec "INSERT INTO facts VALUES ('F59','Le disque dur fait des bruits anormaux (cliquetis, grincements)','Disque');")
  (sql-exec "INSERT INTO facts VALUES ('F60','L''espace disponible sur le disque est insuffisant','Disque');")
  (sql-exec "INSERT INTO facts VALUES ('F61','Des erreurs de lecture ou d''écriture se produisent','Disque');")
  (sql-exec "INSERT INTO facts VALUES ('F62','Le disque dur n''est pas détecté par l''ordinateur','Disque');")
  (sql-exec "INSERT INTO facts VALUES ('F63','Une partition du disque semble corrompue','Disque');")
  (sql-exec "INSERT INTO facts VALUES ('F64','Le disque dur a besoin d''être défragmenté','Disque');")
  (sql-exec "INSERT INTO facts VALUES ('F65','Mon SSD est détecté comme un disque dur classique','Disque');")

  ;; Catégorie: Matériel
  (sql-exec "INSERT INTO facts VALUES ('F13','Mon moniteur fonctionne correctement avec un autre appareil','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F14','Un autre écran ne fonctionne pas non plus sur ce PC','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F15','Des bips ou des LED clignotent au démarrage','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F16','Une carte graphique dédiée est installée','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F17','La mémoire RAM n''a pas été vérifiée récemment','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F66','Mon clavier ne fonctionne pas ou certaines touches ne répondent pas','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F67','Ma souris ne fonctionne pas ou bouge de manière erratique','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F68','Les ports USB ne fonctionnent plus','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F69','La carte son n''est pas détectée','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F70','La batterie de mon portable ne se charge plus','Matériel');")
  (sql-exec "INSERT INTO facts VALUES ('F71','Ma webcam ne fonctionne pas','Matériel');")

  ;; Catégorie: Système
  (sql-exec "INSERT INTO facts VALUES ('F18','Les mises à jour Windows sont à jour','Système');")
  (sql-exec "INSERT INTO facts VALUES ('F21','Le mode économie d''énergie est activé','Système');")
  (sql-exec "INSERT INTO facts VALUES ('F22','Les paramètres du BIOS ont été modifiés récemment','Système');")
  (sql-exec "INSERT INTO facts VALUES ('F72','Windows semble corrompu ou instable','Système');")
  (sql-exec "INSERT INTO facts VALUES ('F73','Les pilotes de périphériques sont obsolètes','Système');")
  (sql-exec "INSERT INTO facts VALUES ('F74','Certains services Windows sont arrêtés','Système');")
  (sql-exec "INSERT INTO facts VALUES ('F75','Le registre Windows semble corrompu','Système');")
  (sql-exec "INSERT INTO facts VALUES ('F76','Des fichiers système sont manquants','Système');")
  (sql-exec "INSERT INTO facts VALUES ('F77','Mon profil utilisateur semble corrompu','Système');")

  ;; Catégorie: Logiciels
  (sql-exec "INSERT INTO facts VALUES ('F78','Un logiciel plante fréquemment','Logiciels');")
  (sql-exec "INSERT INTO facts VALUES ('F79','Une erreur survient lors de l''installation d''un logiciel','Logiciels');")
  (sql-exec "INSERT INTO facts VALUES ('F80','Un logiciel refuse de se lancer','Logiciels');")
  (sql-exec "INSERT INTO facts VALUES ('F81','Un logiciel n''est pas compatible avec ma version de Windows','Logiciels');")
  (sql-exec "INSERT INTO facts VALUES ('F82','Un logiciel demande des droits administrateur pour fonctionner','Logiciels');")
  (sql-exec "INSERT INTO facts VALUES ('F83','Des dépendances ou composants sont manquants','Logiciels');")
  (sql-exec "INSERT INTO facts VALUES ('F84','Deux logiciels semblent entrer en conflit','Logiciels');")

  ;; Catégorie: Périphériques
  (sql-exec "INSERT INTO facts VALUES ('F85','Mon imprimante ne fonctionne pas','Périphériques');")
  (sql-exec "INSERT INTO facts VALUES ('F86','Mon scanner n''est pas accessible','Périphériques');")
  (sql-exec "INSERT INTO facts VALUES ('F87','Ma clé USB n''est pas reconnue','Périphériques');")
  (sql-exec "INSERT INTO facts VALUES ('F88','Mon disque dur externe n''est pas détecté','Périphériques');")
  (sql-exec "INSERT INTO facts VALUES ('F89','Mon casque audio ne produit aucun son','Périphériques');")
  (sql-exec "INSERT INTO facts VALUES ('F90','Mon microphone ne capte pas ma voix','Périphériques');")

  ;; Catégorie: Audio
  (sql-exec "INSERT INTO facts VALUES ('F91','Je n''ai aucun son sur mon ordinateur','Audio');")
  (sql-exec "INSERT INTO facts VALUES ('F92','Le son est déformé, craquant ou grésillant','Audio');")
  (sql-exec "INSERT INTO facts VALUES ('F93','Le volume sonore est trop faible','Audio');")
  (sql-exec "INSERT INTO facts VALUES ('F94','Le son ne sort que d''un seul côté (gauche ou droite)','Audio');")
  (sql-exec "INSERT INTO facts VALUES ('F95','J''entends un écho ou un retour de mon microphone','Audio');")

  ;; Catégorie: Affichage
  (sql-exec "INSERT INTO facts VALUES ('F96','La résolution de mon écran est incorrecte','Affichage');")
  (sql-exec "INSERT INTO facts VALUES ('F97','L''image à l''écran est floue','Affichage');")
  (sql-exec "INSERT INTO facts VALUES ('F98','Les couleurs sont déformées ou incorrectes','Affichage');")
  (sql-exec "INSERT INTO facts VALUES ('F99','L''écran clignote ou scintille','Affichage');")
  (sql-exec "INSERT INTO facts VALUES ('F100','Mes écrans supplémentaires ne sont pas détectés','Affichage');")

  ;; RULES - Beaucoup plus de règles et combinaisons

  ;; Règles Performance (multi-faits -> PRECIS)
  (sql-exec "INSERT INTO rules VALUES ('R1','F1;F7','CAUSE_POSSIBLE(pc_lent,surcharge_logicielle_processus_gourmands)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R2','CAUSE_POSSIBLE(pc_lent,surcharge_logicielle_processus_gourmands)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Vos symptômes indiquent une surcharge logicielle. Ouvrez le Gestionnaire des tâches (Ctrl+Maj+Echap) pour identifier et fermer les applications qui consomment trop de ressources. Désactivez également les programmes inutiles au démarrage.)','J''ai fermé les applications gourmandes et désactivé des programmes au démarrage');")

  (sql-exec "INSERT INTO rules VALUES ('R3','F1;F8','CAUSE_POSSIBLE(pc_lent,conflit_multiscan_antivirus)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R4','CAUSE_POSSIBLE(pc_lent,conflit_multiscan_antivirus)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Conflit antivirus détecté ! Conservez un seul antivirus sur votre ordinateur. Avoir plusieurs antivirus actifs provoque des conflits et ralentit fortement le système.)','J''ai désinstallé les antivirus en trop pour n''en garder qu''un seul');")

  (sql-exec "INSERT INTO rules VALUES ('R5','F1;F9','CAUSE_POSSIBLE(pc_lent,infection_malware)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R6','CAUSE_POSSIBLE(pc_lent,infection_malware)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Infection probable ! Lancez une analyse complète avec votre antivirus. Téléchargez et exécutez également Malwarebytes pour une détection approfondie des logiciels malveillants.)','J''ai lancé une analyse antivirus complète et utilisé Malwarebytes');")

  (sql-exec "INSERT INTO rules VALUES ('R7','F1;F10','CAUSE_POSSIBLE(pc_lent,disque_dur_degrade)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R8','CAUSE_POSSIBLE(pc_lent,disque_dur_degrade)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Disque défaillant détecté ! Sauvegardez immédiatement vos données importantes et envisagez de remplacer le disque par un SSD pour de meilleures performances.)','J''ai sauvegardé mes données et/ou remplacé le disque dur');")

  (sql-exec "INSERT INTO rules VALUES ('R9','F1;F11;F12','CAUSE_POSSIBLE(pc_lent,surchauffe_thermique)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R10','CAUSE_POSSIBLE(pc_lent,surchauffe_thermique)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Surchauffe confirmée ! Nettoyez les ventilateurs et grilles d''aération avec de l''air comprimé. Si le problème persiste, la pâte thermique du processeur doit être remplacée.)','J''ai nettoyé les ventilateurs et les grilles d''aération');")

  (sql-exec "INSERT INTO rules VALUES ('R11','F1;F21','CAUSE_POSSIBLE(pc_lent,mode_economie_d_energie)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R12','CAUSE_POSSIBLE(pc_lent,mode_economie_d_energie)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Mode économie d''énergie actif ! Allez dans Paramètres > Système > Alimentation et changez le mode d''alimentation sur « Équilibré » ou « Performances élevées ».)','J''ai changé le mode d''alimentation sur Équilibré ou Performances élevées');")

  (sql-exec "INSERT INTO rules VALUES ('R13','F1;F22','CAUSE_POSSIBLE(pc_lent,parametres_BIOS_invalides)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R14','CAUSE_POSSIBLE(pc_lent,parametres_BIOS_invalides)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Configuration BIOS suspecte ! Redémarrez l''ordinateur et accédez au BIOS (touche DEL, F2 ou F10 au démarrage). Chargez les paramètres par défaut (Load Optimized Defaults).)','J''ai réinitialisé les paramètres du BIOS par défaut');")

  (sql-exec "INSERT INTO rules VALUES ('R15','F1;F30','CAUSE_POSSIBLE(pc_lent,memoire_ram_insuffisante)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R16','CAUSE_POSSIBLE(pc_lent,memoire_ram_insuffisante)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Mémoire RAM saturée ! Fermez les applications inutiles ou envisagez d''ajouter des barrettes de RAM supplémentaires.)','J''ai fermé des applications pour libérer la RAM ou ajouté de la mémoire');")

  (sql-exec "INSERT INTO rules VALUES ('R17','F1;F31','CAUSE_POSSIBLE(pc_lent,pagination_excessive)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R18','CAUSE_POSSIBLE(pc_lent,pagination_excessive)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Pagination excessive détectée ! Le système utilise trop le fichier d''échange car la RAM est saturée. Augmentez la mémoire RAM ou réduisez le nombre d''applications ouvertes.)','J''ai réduit le nombre d''applications ouvertes ou augmenté la RAM');")

  (sql-exec "INSERT INTO rules VALUES ('R19','F1;F60','CAUSE_POSSIBLE(pc_lent,espace_disque_plein)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R20','CAUSE_POSSIBLE(pc_lent,espace_disque_plein)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Disque plein ! Libérez de l''espace en supprimant les fichiers inutiles. Utilisez l''outil Nettoyage de disque Windows (tapez cleanmgr dans la recherche).)','J''ai libéré de l''espace disque en supprimant des fichiers inutiles');")

  (sql-exec "INSERT INTO rules VALUES ('R21','F1;F64','CAUSE_POSSIBLE(pc_lent,disque_fragmente)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R22','CAUSE_POSSIBLE(pc_lent,disque_fragmente)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Fragmentation détectée ! Défragmentez votre disque dur via l''outil Windows (Optimiser les lecteurs). Note : ne pas défragmenter un SSD.)','J''ai défragmenté mon disque dur');")

  (sql-exec "INSERT INTO rules VALUES ('R23','F1;F73','CAUSE_POSSIBLE(pc_lent,pilotes_obsoletes)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R24','CAUSE_POSSIBLE(pc_lent,pilotes_obsoletes)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Pilotes obsolètes détectés ! Mettez à jour vos pilotes via Windows Update ou téléchargez-les depuis le site du fabricant de votre ordinateur.)','J''ai mis à jour mes pilotes via Windows Update ou le site du fabricant');")

  (sql-exec "INSERT INTO rules VALUES ('R25','F1;F75','CAUSE_POSSIBLE(pc_lent,registre_corrompu)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R26','CAUSE_POSSIBLE(pc_lent,registre_corrompu)','ACTION_RECOMMANDEE(pc_lent,PRECIS:Registre Windows corrompu ! Exécutez la commande sfc /scannow dans une invite de commandes administrateur pour réparer les fichiers système.)','J''ai exécuté sfc /scannow pour réparer les fichiers système');")

  ;; NOUVELLES RÈGLES COMPLEXES - Performance (3-4 faits)
  (sql-exec "INSERT INTO rules VALUES ('R200','F1;F7;F30;F31','CAUSE_POSSIBLE(pc_lent,saturation_complete_systeme)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R201','CAUSE_POSSIBLE(pc_lent,saturation_complete_systeme)','ACTION_RECOMMANDEE(pc_lent,PRECIS:SATURATION CRITIQUE ! Votre système manque de RAM et le processeur est surchargé. Solution urgente : fermez toutes les applications non essentielles, puis envisagez sérieusement d''augmenter la RAM (8 Go minimum recommandé).)','J''ai fermé les applications non essentielles et/ou augmenté la RAM');")

  (sql-exec "INSERT INTO rules VALUES ('R202','F1;F10;F59;F61','CAUSE_POSSIBLE(pc_lent,disque_en_fin_de_vie)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R203','CAUSE_POSSIBLE(pc_lent,disque_en_fin_de_vie)','ACTION_RECOMMANDEE(pc_lent,PRECIS:ALERTE DISQUE CRITIQUE ! Bruits anormaux + erreurs S.M.A.R.T. + erreurs lecture = disque en fin de vie. SAUVEGARDEZ VOS DONNÉES IMMÉDIATEMENT et remplacez le disque au plus vite.)','J''ai sauvegardé mes données et remplacé le disque défaillant');")

  (sql-exec "INSERT INTO rules VALUES ('R204','F1;F11;F48;F47','CAUSE_POSSIBLE(pc_lent,defaillance_refroidissement_critique)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R205','CAUSE_POSSIBLE(pc_lent,defaillance_refroidissement_critique)','ACTION_RECOMMANDEE(pc_lent,PRECIS:SURCHAUFFE DANGEREUSE ! Le ventilateur ne fonctionne pas et l''ordinateur s''éteint tout seul. Cessez d''utiliser le PC et remplacez le ventilateur pour éviter d''endommager le processeur.)','J''ai remplacé le ventilateur défectueux');")

  (sql-exec "INSERT INTO rules VALUES ('R206','F1;F9;F52;F53','CAUSE_POSSIBLE(pc_lent,infection_severe)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R207','CAUSE_POSSIBLE(pc_lent,infection_severe)','ACTION_RECOMMANDEE(pc_lent,PRECIS:INFECTION SÉVÈRE ! Malware détecté avec pop-ups et redirections. Déconnectez-vous d''internet, lancez une analyse avec Malwarebytes en mode sans échec, puis réinitialisez vos navigateurs.)','J''ai nettoyé l''infection avec Malwarebytes et réinitialisé mes navigateurs');")

  ;; Règles Démarrage (multi-faits -> PRECIS)
  (sql-exec "INSERT INTO rules VALUES ('R27','F3;F13;F14','CAUSE_POSSIBLE(pc_demarre_sans_affichage,probleme_sortie_video)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R28','CAUSE_POSSIBLE(pc_demarre_sans_affichage,probleme_sortie_video);F16','ACTION_RECOMMANDEE(pc_demarre_sans_affichage,PRECIS:Problème de sortie vidéo identifié ! Branchez votre écran sur la sortie vidéo intégrée de la carte mère au lieu de la carte graphique dédiée pour tester.)','J''ai testé en branchant l''écran sur la sortie vidéo de la carte mère');")

  (sql-exec "INSERT INTO rules VALUES ('R29','F3;F15','CAUSE_POSSIBLE(pc_demarre_sans_affichage,defaut_memoire_ou_carte_mere)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R30','CAUSE_POSSIBLE(pc_demarre_sans_affichage,defaut_memoire_ou_carte_mere);F17','ACTION_RECOMMANDEE(pc_demarre_sans_affichage,PRECIS:Défaut mémoire probable ! Éteignez le PC, retirez et réinsérez les barrettes de RAM. Testez avec une seule barrette si possible.)','J''ai retiré et réinséré les barrettes de RAM');")

  (sql-exec "INSERT INTO rules VALUES ('R31','F2','CAUSE_POSSIBLE(pc_ne_s_allume_pas,probleme_alimentation)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R32','CAUSE_POSSIBLE(pc_ne_s_allume_pas,probleme_alimentation)','ACTION_RECOMMANDEE(pc_ne_s_allume_pas,GENERAL:Vérifiez que le câble d''alimentation est bien branché et que la multiprise fonctionne. Testez sur une autre prise électrique.)','J''ai vérifié le câble d''alimentation et testé sur une autre prise');")

  (sql-exec "INSERT INTO rules VALUES ('R33','F6','CAUSE_POSSIBLE(pc_bloque_demarrage,probleme_BIOS_ou_OS)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R34','CAUSE_POSSIBLE(pc_bloque_demarrage,probleme_BIOS_ou_OS)','ACTION_RECOMMANDEE(pc_bloque_demarrage,GENERAL:Vérifiez l''ordre de démarrage dans le BIOS. Tentez une réparation automatique de Windows via une clé USB d''installation.)','J''ai vérifié l''ordre de démarrage et/ou lancé une réparation Windows');")

  (sql-exec "INSERT INTO rules VALUES ('R35','F32;F30','CAUSE_POSSIBLE(demarrage_lent,ram_pleine_des_demarrage)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R36','CAUSE_POSSIBLE(demarrage_lent,ram_pleine_des_demarrage)','ACTION_RECOMMANDEE(demarrage_lent,PRECIS:RAM saturée au démarrage ! Désactivez les programmes au démarrage : Ctrl+Maj+Echap > onglet Démarrage > Désactiver les applications inutiles.)','J''ai désactivé des programmes au démarrage');")

  (sql-exec "INSERT INTO rules VALUES ('R37','F32;F10','CAUSE_POSSIBLE(demarrage_lent,disque_lent)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R38','CAUSE_POSSIBLE(demarrage_lent,disque_lent)','ACTION_RECOMMANDEE(demarrage_lent,PRECIS:Disque défaillant détecté ! Votre disque dur est lent et montre des signes de défaillance. Remplacez-le par un SSD pour des performances nettement améliorées.)','J''ai remplacé mon disque dur par un SSD');")

  (sql-exec "INSERT INTO rules VALUES ('R39','F33;F72','CAUSE_POSSIBLE(erreur_demarrage,windows_corrompu)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R40','CAUSE_POSSIBLE(erreur_demarrage,windows_corrompu)','ACTION_RECOMMANDEE(erreur_demarrage,PRECIS:Windows corrompu ! Démarrez sur une clé USB Windows et choisissez Réparer l''ordinateur > Dépannage > Réparation automatique.)','J''ai lancé la réparation automatique de Windows');")

  (sql-exec "INSERT INTO rules VALUES ('R41','F34;F72','CAUSE_POSSIBLE(boucle_redemarrage,windows_corrompu)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R42','CAUSE_POSSIBLE(boucle_redemarrage,windows_corrompu)','ACTION_RECOMMANDEE(boucle_redemarrage,PRECIS:Boucle de redémarrage + Windows instable ! Démarrez en mode sans échec (Maj+Redémarrer) et utilisez la restauration du système.)','J''ai utilisé la restauration du système en mode sans échec');")

  (sql-exec "INSERT INTO rules VALUES ('R43','F35;F73','CAUSE_POSSIBLE(bsod_demarrage,pilotes_defectueux)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R44','CAUSE_POSSIBLE(bsod_demarrage,pilotes_defectueux)','ACTION_RECOMMANDEE(bsod_demarrage,PRECIS:Écran bleu + pilotes obsolètes ! Démarrez en mode sans échec et mettez à jour ou réinstallez les pilotes récemment modifiés.)','J''ai mis à jour ou réinstallé les pilotes en mode sans échec');")

  ;; NOUVELLES RÈGLES COMPLEXES - Démarrage (3-4 faits)
  (sql-exec "INSERT INTO rules VALUES ('R208','F3;F15;F37;F36','CAUSE_POSSIBLE(demarrage,panne_materielle_grave)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R209','CAUSE_POSSIBLE(demarrage,panne_materielle_grave)','ACTION_RECOMMANDEE(demarrage,PRECIS:PANNE MATÉRIELLE ! Écran noir + bips + ventilateur qui tourne + BIOS invisible = problème de carte mère ou processeur. Consultez un technicien.)','J''ai consulté un technicien pour la panne matérielle');")

  (sql-exec "INSERT INTO rules VALUES ('R210','F32;F30;F7;F60','CAUSE_POSSIBLE(demarrage,systeme_sature)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R211','CAUSE_POSSIBLE(demarrage,systeme_sature)','ACTION_RECOMMANDEE(demarrage,PRECIS:SYSTÈME COMPLÈTEMENT SATURÉ ! Démarrage lent + RAM pleine + CPU à 80% + disque plein. Nettoyez le disque, fermez les applications au démarrage, et envisagez d''ajouter de la RAM.)','J''ai nettoyé le disque, désactivé des programmes au démarrage et/ou ajouté de la RAM');")

  (sql-exec "INSERT INTO rules VALUES ('R212','F34;F35;F33','CAUSE_POSSIBLE(demarrage,windows_gravement_endommage)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R213','CAUSE_POSSIBLE(demarrage,windows_gravement_endommage)','ACTION_RECOMMANDEE(demarrage,PRECIS:WINDOWS GRAVEMENT ENDOMMAGÉ ! Boucle de redémarrage + écran bleu + erreurs = réinstallation de Windows probablement nécessaire. Sauvegardez vos données d''abord.)','J''ai réinstallé Windows après avoir sauvegardé mes données');")

  ;; Règles Réseau (multi-faits -> PRECIS)
  (sql-exec "INSERT INTO rules VALUES ('R45','F4;F18','CAUSE_POSSIBLE(wifi,systeme_non_a_jour)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R46','CAUSE_POSSIBLE(wifi,systeme_non_a_jour)','ACTION_RECOMMANDEE(wifi,Installez toutes les mises à jour Windows disponibles via Paramètres > Windows Update.)','J''ai installé toutes les mises à jour Windows');")

  (sql-exec "INSERT INTO rules VALUES ('R47','F4;F19','CAUSE_POSSIBLE(wifi,probleme_logiciel_carte_reseau)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R48','CAUSE_POSSIBLE(wifi,probleme_logiciel_carte_reseau)','ACTION_RECOMMANDEE(wifi,PRECIS:Carte réseau détectée mais WiFi KO ! Allez dans le Gestionnaire de périphériques, faites clic droit sur votre carte réseau et choisissez Désactiver, puis Réactiver.)','J''ai désactivé puis réactivé ma carte réseau');")

  (sql-exec "INSERT INTO rules VALUES ('R49','F4;F20','CAUSE_POSSIBLE(wifi,conflit_antivirus_sur_pile_reseau)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R50','CAUSE_POSSIBLE(wifi,conflit_antivirus_sur_pile_reseau)','ACTION_RECOMMANDEE(wifi,PRECIS:Conflit antivirus/réseau ! Désinstallez temporairement votre antivirus tiers pour vérifier s''il bloque la connexion réseau.)','J''ai désinstallé temporairement mon antivirus tiers');")

  (sql-exec "INSERT INTO rules VALUES ('R51','F4','ACTION_RECOMMANDEE(wifi,GENERAL:Ouvrez une invite de commandes administrateur et tapez : netsh winsock reset puis netsh int ip reset. Redémarrez ensuite votre PC.)','J''ai réinitialisé la pile réseau avec netsh');")

  (sql-exec "INSERT INTO rules VALUES ('R52','F39;F19','CAUSE_POSSIBLE(ethernet,probleme_cable_ou_port)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R53','CAUSE_POSSIBLE(ethernet,probleme_cable_ou_port)','ACTION_RECOMMANDEE(ethernet,PRECIS:Carte détectée mais Ethernet KO ! Vérifiez le câble Ethernet des deux côtés. Essayez un autre câble ou un autre port sur votre routeur.)','J''ai vérifié/changé le câble Ethernet ou testé un autre port');")

  (sql-exec "INSERT INTO rules VALUES ('R54','F40;F57','CAUSE_POSSIBLE(wifi_instable,parefeu_bloquant)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R55','CAUSE_POSSIBLE(wifi_instable,parefeu_bloquant)','ACTION_RECOMMANDEE(wifi_instable,PRECIS:WiFi instable + pare-feu désactivé ! Vérifiez les paramètres du pare-feu Windows et ajoutez des exceptions pour vos applications réseau.)','J''ai configuré le pare-feu et ajouté des exceptions');")

  (sql-exec "INSERT INTO rules VALUES ('R56','F41;F52','CAUSE_POSSIBLE(debit_lent,infection_malware)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R57','CAUSE_POSSIBLE(debit_lent,infection_malware)','ACTION_RECOMMANDEE(debit_lent,PRECIS:Connexion lente + pop-ups = malware probable ! Lancez une analyse complète avec votre antivirus et Malwarebytes.)','J''ai lancé une analyse antivirus et Malwarebytes');")

  (sql-exec "INSERT INTO rules VALUES ('R58','F42;F58','CAUSE_POSSIBLE(sites_inaccessibles,mises_a_jour_manquees)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R59','CAUSE_POSSIBLE(sites_inaccessibles,mises_a_jour_manquees)','ACTION_RECOMMANDEE(sites_inaccessibles,PRECIS:Sites inaccessibles + mises à jour manquantes ! Installez les mises à jour de sécurité Windows pour les certificats de sécurité.)','J''ai installé les mises à jour de sécurité Windows');")

  ;; NOUVELLES RÈGLES COMPLEXES - Réseau (3-4 faits)
  (sql-exec "INSERT INTO rules VALUES ('R214','F4;F39;F46','CAUSE_POSSIBLE(reseau,configuration_ip_invalide)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R215','CAUSE_POSSIBLE(reseau,configuration_ip_invalide)','ACTION_RECOMMANDEE(reseau,PRECIS:WiFi + Ethernet KO + IP invalide = problème DHCP ! Redémarrez votre box internet et exécutez ipconfig /release puis ipconfig /renew.)','J''ai redémarré ma box et renouvelé mon adresse IP');")

  (sql-exec "INSERT INTO rules VALUES ('R216','F40;F41;F24','CAUSE_POSSIBLE(reseau,probleme_connexion_general)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R217','CAUSE_POSSIBLE(reseau,probleme_connexion_general)','ACTION_RECOMMANDEE(reseau,PRECIS:WiFi instable + débit lent + navigation lente = problème de connexion global. Contactez votre fournisseur d''accès ou changez de canal WiFi sur votre box.)','J''ai contacté mon FAI et/ou changé le canal WiFi');")

  (sql-exec "INSERT INTO rules VALUES ('R218','F4;F40;F20;F57','CAUSE_POSSIBLE(reseau,securite_bloque_reseau)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R219','CAUSE_POSSIBLE(reseau,securite_bloque_reseau)','ACTION_RECOMMANDEE(reseau,PRECIS:CONFLIT SÉCURITÉ ! Antivirus tiers + pare-feu désactivé + problèmes WiFi = votre suite de sécurité bloque le réseau. Désinstallez l''antivirus tiers et utilisez Windows Defender.)','J''ai désinstallé l''antivirus tiers et activé Windows Defender');")

  ;; Règles Thermique (multi-faits -> PRECIS)
  (sql-exec "INSERT INTO rules VALUES ('R60','F5;F11;F12','CAUSE_POSSIBLE(surchauffe,encrassement_systeme_refroidissement)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R61','CAUSE_POSSIBLE(surchauffe,encrassement_systeme_refroidissement)','ACTION_RECOMMANDEE(surchauffe,PRECIS:Surchauffe + aérations obstruées ! Utilisez de l''air comprimé pour nettoyer la poussière accumulée dans les ventilateurs et sur les dissipateurs.)','J''ai nettoyé la poussière des ventilateurs et dissipateurs');")

  (sql-exec "INSERT INTO rules VALUES ('R62','F47;F11','CAUSE_POSSIBLE(arret_automatique,surchauffe_critique)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R63','CAUSE_POSSIBLE(arret_automatique,surchauffe_critique)','ACTION_RECOMMANDEE(arret_automatique,PRECIS:Arrêts automatiques + température élevée ! Votre PC s''éteint pour se protéger. Nettoyez le refroidissement et vérifiez les ventilateurs.)','J''ai nettoyé le système de refroidissement et vérifié les ventilateurs');")

  (sql-exec "INSERT INTO rules VALUES ('R64','F48;F11','CAUSE_POSSIBLE(ventilateur_hs,surchauffe)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R65','CAUSE_POSSIBLE(ventilateur_hs,surchauffe)','ACTION_RECOMMANDEE(ventilateur_hs,PRECIS:Ventilateur arrêté + température élevée ! Le ventilateur défectueux doit être remplacé pour éviter d''endommager le processeur.)','J''ai remplacé le ventilateur défectueux');")

  ;; NOUVELLES RÈGLES COMPLEXES - Thermique (3-4 faits)
  (sql-exec "INSERT INTO rules VALUES ('R220','F5;F11;F48;F50','CAUSE_POSSIBLE(thermique,defaillance_complete_refroidissement)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R221','CAUSE_POSSIBLE(thermique,defaillance_complete_refroidissement)','ACTION_RECOMMANDEE(thermique,PRECIS:URGENCE THERMIQUE ! Surchauffe + température élevée + ventilateur HS + throttling = système de refroidissement défaillant. Cessez d''utiliser le PC et remplacez le ventilateur.)','J''ai remplacé le système de refroidissement défaillant');")

  (sql-exec "INSERT INTO rules VALUES ('R222','F47;F49;F11;F1','CAUSE_POSSIBLE(thermique,surchauffe_degradant_performances)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R223','CAUSE_POSSIBLE(thermique,surchauffe_degradant_performances)','ACTION_RECOMMANDEE(thermique,PRECIS:CERCLE VICIEUX THERMIQUE ! PC lent + chaud au toucher + arrêts + température élevée. Nettoyez d''urgence le système de refroidissement et appliquez de nouvelle pâte thermique.)','J''ai nettoyé le refroidissement et appliqué de la nouvelle pâte thermique');")

  ;; Règles Sécurité (multi-faits -> PRECIS)
  (sql-exec "INSERT INTO rules VALUES ('R66','F52;F53','CAUSE_POSSIBLE(publicites_incessantes,adware_installe)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R67','CAUSE_POSSIBLE(publicites_incessantes,adware_installe)','ACTION_RECOMMANDEE(publicites_incessantes,PRECIS:Adware détecté ! Pop-ups + redirections = publiciels installés. Téléchargez AdwCleaner puis Malwarebytes pour les supprimer.)','J''ai utilisé AdwCleaner et Malwarebytes pour supprimer les adwares');")

  (sql-exec "INSERT INTO rules VALUES ('R68','F54;F55','CAUSE_POSSIBLE(ransomware,infection_critique)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R69','CAUSE_POSSIBLE(ransomware,infection_critique)','ACTION_RECOMMANDEE(ransomware,PRECIS:RANSOMWARE DÉTECTÉ ! Fichiers cryptés + mot de passe changé = URGENT : Déconnectez d''internet. Ne payez pas la rançon. Contactez un expert en cybersécurité.)','J''ai contacté un expert en cybersécurité pour le ransomware');")

  (sql-exec "INSERT INTO rules VALUES ('R70','F56;F57','CAUSE_POSSIBLE(logiciels_inconnus,parefeu_desactive)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R71','CAUSE_POSSIBLE(logiciels_inconnus,parefeu_desactive)','ACTION_RECOMMANDEE(logiciels_inconnus,PRECIS:Logiciels suspects + pare-feu désactivé ! Activez le pare-feu Windows et lancez une analyse antivirus complète.)','J''ai activé le pare-feu et lancé une analyse antivirus');")

  ;; NOUVELLES RÈGLES COMPLEXES - Sécurité (3-4 faits)
  (sql-exec "INSERT INTO rules VALUES ('R224','F9;F52;F53;F56','CAUSE_POSSIBLE(securite,infection_multiple)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R225','CAUSE_POSSIBLE(securite,infection_multiple)','ACTION_RECOMMANDEE(securite,PRECIS:INFECTION GRAVE MULTIPLE ! Malware + pop-ups + redirections + logiciels inconnus. Démarrez en mode sans échec, lancez Malwarebytes, puis réinitialisez vos navigateurs.)','J''ai nettoyé l''infection en mode sans échec avec Malwarebytes');")

  (sql-exec "INSERT INTO rules VALUES ('R226','F54;F55;F9;F57','CAUSE_POSSIBLE(securite,compromission_totale)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R227','CAUSE_POSSIBLE(securite,compromission_totale)','ACTION_RECOMMANDEE(securite,PRECIS:SYSTÈME COMPROMIS ! Ransomware + malware + pare-feu désactivé + mot de passe changé. FORMATEZ le disque et réinstallez Windows. Changez tous vos mots de passe depuis un autre appareil.)','J''ai formaté et réinstallé Windows, puis changé tous mes mots de passe');")

  ;; Règles Disque (multi-faits -> PRECIS)
  (sql-exec "INSERT INTO rules VALUES ('R72','F59;F10','CAUSE_POSSIBLE(disque_bruyant,disque_defectueux)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R73','CAUSE_POSSIBLE(disque_bruyant,disque_defectueux)','ACTION_RECOMMANDEE(disque_bruyant,PRECIS:Bruits anormaux + erreurs S.M.A.R.T. = défaillance imminente ! Sauvegardez vos données immédiatement et remplacez le disque.)','J''ai sauvegardé mes données et remplacé le disque défectueux');")

  (sql-exec "INSERT INTO rules VALUES ('R74','F61;F63','CAUSE_POSSIBLE(erreurs_lecture,partition_corrompue)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R75','CAUSE_POSSIBLE(erreurs_lecture,partition_corrompue)','ACTION_RECOMMANDEE(erreurs_lecture,PRECIS:Erreurs lecture + partition corrompue ! Exécutez chkdsk C: /f /r dans une invite de commandes administrateur.)','J''ai exécuté chkdsk pour réparer les erreurs du disque');")

  (sql-exec "INSERT INTO rules VALUES ('R76','F62;F15','CAUSE_POSSIBLE(disque_non_detecte,defaut_materiel)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R77','CAUSE_POSSIBLE(disque_non_detecte,defaut_materiel)','ACTION_RECOMMANDEE(disque_non_detecte,Vérifiez les connexions des câbles SATA et d''alimentation du disque. Testez avec d''autres câbles si possible.)','J''ai vérifié et/ou changé les câbles SATA et d''alimentation');")

  ;; Règles Matériel
  (sql-exec "INSERT INTO rules VALUES ('R78','F66;F38','CAUSE_POSSIBLE(clavier_hs,defaut_materiel)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R79','CAUSE_POSSIBLE(clavier_hs,defaut_materiel)','ACTION_RECOMMANDEE(clavier_hs,Branchez un autre clavier USB pour vérifier si le problème vient du clavier ou de l''ordinateur.)','J''ai testé avec un autre clavier USB');")

  (sql-exec "INSERT INTO rules VALUES ('R80','F67;F38','CAUSE_POSSIBLE(souris_hs,defaut_materiel)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R81','CAUSE_POSSIBLE(souris_hs,defaut_materiel)','ACTION_RECOMMANDEE(souris_hs,Branchez une autre souris USB pour vérifier si le problème vient de la souris ou de l''ordinateur.)','J''ai testé avec une autre souris USB');")

  (sql-exec "INSERT INTO rules VALUES ('R82','F68;F15','CAUSE_POSSIBLE(usb_hs,ports_defectueux)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R83','CAUSE_POSSIBLE(usb_hs,ports_defectueux)','ACTION_RECOMMANDEE(usb_hs,Testez les ports USB à l''arrière de l''unité centrale, ils sont souvent plus fiables que ceux en façade.)','J''ai testé les ports USB à l''arrière de l''ordinateur');")

  (sql-exec "INSERT INTO rules VALUES ('R84','F70;F2','CAUSE_POSSIBLE(batterie_hs,defaut_alimentation)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R85','CAUSE_POSSIBLE(batterie_hs,defaut_alimentation)','ACTION_RECOMMANDEE(batterie_hs,La batterie est probablement en fin de vie. Remplacez-la par une batterie neuve compatible avec votre modèle.)','J''ai remplacé la batterie par une neuve');")

  ;; Règles Logiciels
  (sql-exec "INSERT INTO rules VALUES ('R86','F78;F81','CAUSE_POSSIBLE(logiciel_plante,incompatibilite_os)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R87','CAUSE_POSSIBLE(logiciel_plante,incompatibilite_os)','ACTION_RECOMMANDEE(logiciel_plante,Vérifiez la compatibilité du logiciel avec votre version de Windows. Mettez-le à jour ou essayez le mode de compatibilité.)','J''ai mis à jour le logiciel ou activé le mode de compatibilité');")

  (sql-exec "INSERT INTO rules VALUES ('R88','F79;F82','CAUSE_POSSIBLE(installation_echoue,droits_admin_manquants)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R89','CAUSE_POSSIBLE(installation_echoue,droits_admin_manquants)','ACTION_RECOMMANDEE(installation_echoue,Faites clic droit sur le fichier d''installation et choisissez Exécuter en tant qu''administrateur.)','J''ai exécuté l''installation en tant qu''administrateur');")

  (sql-exec "INSERT INTO rules VALUES ('R90','F80;F83','CAUSE_POSSIBLE(logiciel_ne_demarre_pas,dependances_manquantes)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R91','CAUSE_POSSIBLE(logiciel_ne_demarre_pas,dependances_manquantes)','ACTION_RECOMMANDEE(logiciel_ne_demarre_pas,Installez les composants requis : Visual C++ Redistributable, .NET Framework ou DirectX selon les besoins du logiciel.)','J''ai installé les composants requis (Visual C++, .NET, DirectX)');")

  ;; Règles Périphériques
  (sql-exec "INSERT INTO rules VALUES ('R92','F85;F44','CAUSE_POSSIBLE(imprimante_hs,reseau_imprimante)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R93','CAUSE_POSSIBLE(imprimante_hs,reseau_imprimante)','ACTION_RECOMMANDEE(imprimante_hs,Vérifiez que l''imprimante est allumée et connectée au même réseau que votre ordinateur. Redémarrez-la si nécessaire.)','J''ai vérifié la connexion réseau de l''imprimante et l''ai redémarrée');")

  (sql-exec "INSERT INTO rules VALUES ('R94','F87;F68','CAUSE_POSSIBLE(cle_usb_hs,ports_usb_defectueux)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R95','CAUSE_POSSIBLE(cle_usb_hs,ports_usb_defectueux)','ACTION_RECOMMANDEE(cle_usb_hs,Testez la clé USB sur un autre port, de préférence à l''arrière de l''ordinateur. Essayez également sur un autre PC.)','J''ai testé la clé USB sur d''autres ports et/ou un autre PC');")

  ;; Règles Audio
  (sql-exec "INSERT INTO rules VALUES ('R96','F91;F69','CAUSE_POSSIBLE(aucun_son,carte_son_non_detectee)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R97','CAUSE_POSSIBLE(aucun_son,carte_son_non_detectee)','ACTION_RECOMMANDEE(aucun_son,Mettez à jour les pilotes audio via le Gestionnaire de périphériques ou téléchargez-les depuis le site du fabricant.)','J''ai mis à jour les pilotes audio');")

  (sql-exec "INSERT INTO rules VALUES ('R98','F92;F73','CAUSE_POSSIBLE(son_deforme,pilotes_audio_obsoletes)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R99','CAUSE_POSSIBLE(son_deforme,pilotes_audio_obsoletes)','ACTION_RECOMMANDEE(son_deforme,Désinstallez puis réinstallez les pilotes audio. Téléchargez la dernière version depuis le site du fabricant.)','J''ai réinstallé les pilotes audio');")

  ;; Règles Affichage
  (sql-exec "INSERT INTO rules VALUES ('R100','F96;F73','CAUSE_POSSIBLE(resolution_incorrecte,pilotes_graphiques_obsoletes)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R101','CAUSE_POSSIBLE(resolution_incorrecte,pilotes_graphiques_obsoletes)','ACTION_RECOMMANDEE(resolution_incorrecte,Mettez à jour les pilotes de votre carte graphique depuis le site NVIDIA, AMD ou Intel selon votre matériel.)','J''ai mis à jour les pilotes de ma carte graphique');")

  (sql-exec "INSERT INTO rules VALUES ('R102','F97;F98','CAUSE_POSSIBLE(affichage_deforme,cables_defectueux)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R103','CAUSE_POSSIBLE(affichage_deforme,cables_defectueux)','ACTION_RECOMMANDEE(affichage_deforme,PRECIS:Image floue + couleurs déformées = câbles défectueux ! Vérifiez l''état de vos câbles HDMI, DisplayPort ou VGA. Remplacez-les si nécessaire.)','J''ai vérifié et/ou remplacé les câbles d''écran');")

  ;; Règles générales (mono-fait -> GENERAL)
  (sql-exec "INSERT INTO rules VALUES ('R104','F1','ACTION_RECOMMANDEE(pc_lent,GENERAL:Commencez par redémarrer votre ordinateur. Cela peut résoudre de nombreux problèmes de lenteur temporaires.)','J''ai redémarré mon ordinateur');")
  (sql-exec "INSERT INTO rules VALUES ('R105','F4','ACTION_RECOMMANDEE(wifi,GENERAL:Redémarrez votre box internet ainsi que votre ordinateur. Attendez 30 secondes entre chaque redémarrage.)','J''ai redémarré ma box internet et mon ordinateur');")
  (sql-exec "INSERT INTO rules VALUES ('R106','F5','ACTION_RECOMMANDEE(surchauffe,GENERAL:Placez votre ordinateur portable sur une surface plane et dure pour permettre une bonne circulation de l''air.)','J''ai placé mon ordinateur sur une surface plane et dure');")
  (sql-exec "INSERT INTO rules VALUES ('R107','F9','ACTION_RECOMMANDEE(malware,GENERAL:Lancez une analyse complète avec Windows Defender (Sécurité Windows > Protection contre les virus).)','J''ai lancé une analyse avec Windows Defender');")
  (sql-exec "INSERT INTO rules VALUES ('R108','F10','ACTION_RECOMMANDEE(disque_degrade,GENERAL:Installez CrystalDiskInfo (gratuit) pour surveiller l''état de santé de votre disque dur et anticiper les pannes.)','J''ai installé CrystalDiskInfo pour surveiller mon disque');")
  (sql-exec "INSERT INTO rules VALUES ('R109','F23','ACTION_RECOMMANDEE(applications_lentes,GENERAL:Videz le cache et les fichiers temporaires : tapez %temp% dans la barre de recherche et supprimez le contenu du dossier.)','J''ai vidé le cache et les fichiers temporaires');")
  (sql-exec "INSERT INTO rules VALUES ('R110','F24','ACTION_RECOMMANDEE(web_lent,GENERAL:Videz le cache de votre navigateur et désactivez les extensions inutiles qui peuvent ralentir la navigation.)','J''ai vidé le cache du navigateur et désactivé des extensions');")
  (sql-exec "INSERT INTO rules VALUES ('R111','F25','ACTION_RECOMMANDEE(copie_lente,GENERAL:Si vous avez un disque dur classique (HDD), vérifiez sa fragmentation via l''outil Optimiser les lecteurs de Windows.)','J''ai vérifié la fragmentation de mon disque dur');")
  (sql-exec "INSERT INTO rules VALUES ('R112','F26','ACTION_RECOMMANDEE(multitache_impossible,GENERAL:Fermez les applications que vous n''utilisez pas pour libérer de la mémoire RAM et améliorer la fluidité.)','J''ai fermé les applications inutilisées');")
  (sql-exec "INSERT INTO rules VALUES ('R113','F27','ACTION_RECOMMANDEE(gels_frequents,GENERAL:Vérifiez la température de votre processeur et la mémoire RAM disponible dans le Gestionnaire des tâches.)','J''ai vérifié la température et la RAM dans le Gestionnaire des tâches');")
  (sql-exec "INSERT INTO rules VALUES ('R114','F28','ACTION_RECOMMANDEE(defilement_saccade,GENERAL:Mettez à jour vos pilotes graphiques depuis le site du fabricant (NVIDIA, AMD ou Intel).)','J''ai mis à jour mes pilotes graphiques');")
  (sql-exec "INSERT INTO rules VALUES ('R115','F29','ACTION_RECOMMANDEE(reponse_lente,GENERAL:Désactivez les animations Windows : Paramètres > Accessibilité > Effets visuels > désactivez les effets de transparence.)','J''ai désactivé les animations et effets de transparence Windows');")
  (sql-exec "INSERT INTO rules VALUES ('R116','F32','ACTION_RECOMMANDEE(demarrage_lent,GENERAL:Désactivez les programmes inutiles au démarrage : Ctrl+Maj+Echap > onglet Démarrage.)','J''ai désactivé des programmes au démarrage');")
  (sql-exec "INSERT INTO rules VALUES ('R117','F33','ACTION_RECOMMANDEE(erreur_demarrage,GENERAL:Essayez la restauration du système pour revenir à un état antérieur où l''ordinateur fonctionnait correctement.)','J''ai utilisé la restauration du système');")
  (sql-exec "INSERT INTO rules VALUES ('R118','F34','ACTION_RECOMMANDEE(boucle_redemarrage,GENERAL:Démarrez en mode sans échec en maintenant Maj enfoncé pendant le redémarrage, puis choisissez Dépannage.)','J''ai démarré en mode sans échec');")
  (sql-exec "INSERT INTO rules VALUES ('R119','F35','ACTION_RECOMMANDEE(bsod,GENERAL:Notez le code d''erreur affiché sur l''écran bleu et recherchez-le sur internet pour trouver une solution adaptée.)','J''ai noté et recherché le code d''erreur de l''écran bleu');")
  (sql-exec "INSERT INTO rules VALUES ('R120','F36','ACTION_RECOMMANDEE(bios_invisible,GENERAL:Appuyez sur DEL, F2, F10 ou F12 (selon votre PC) dès le démarrage pour accéder au BIOS.)','J''ai accédé au BIOS au démarrage');")
  (sql-exec "INSERT INTO rules VALUES ('R121','F37','ACTION_RECOMMANDEE(ventilateur_sans_ecran,GENERAL:Vérifiez que les barrettes de RAM et la carte graphique sont correctement insérées dans leurs slots.)','J''ai vérifié l''insertion de la RAM et de la carte graphique');")
  (sql-exec "INSERT INTO rules VALUES ('R122','F38','ACTION_RECOMMANDEE(peripheriques_demarrage,GENERAL:Testez avec un clavier et une souris USB externes pour vérifier si le problème vient des périphériques.)','J''ai testé avec un clavier et une souris USB externes');")
  (sql-exec "INSERT INTO rules VALUES ('R123','F39','ACTION_RECOMMANDEE(ethernet_hs,GENERAL:Vérifiez que le câble Ethernet est bien branché et que la connexion est configurée en DHCP automatique.)','J''ai vérifié le câble Ethernet et la configuration DHCP');")
  (sql-exec "INSERT INTO rules VALUES ('R124','F40','ACTION_RECOMMANDEE(wifi_instable,GENERAL:Accédez à l''interface de votre box et changez le canal WiFi pour éviter les interférences avec les réseaux voisins.)','J''ai changé le canal WiFi sur ma box');")
  (sql-exec "INSERT INTO rules VALUES ('R125','F41','ACTION_RECOMMANDEE(debit_lent,GENERAL:Testez votre débit sur speedtest.net pour vérifier si le problème vient de votre connexion ou de votre fournisseur.)','J''ai testé mon débit sur speedtest.net');")
  (sql-exec "INSERT INTO rules VALUES ('R126','F42','ACTION_RECOMMANDEE(sites_inaccessibles,GENERAL:Videz le cache DNS : ouvrez une invite de commandes et tapez ipconfig /flushdns.)','J''ai vidé le cache DNS avec ipconfig /flushdns');")
  (sql-exec "INSERT INTO rules VALUES ('R127','F43','ACTION_RECOMMANDEE(partage_reseau,GENERAL:Activez la découverte réseau dans Paramètres > Réseau et Internet > Options de partage avancées.)','J''ai activé la découverte réseau');")
  (sql-exec "INSERT INTO rules VALUES ('R128','F44','ACTION_RECOMMANDEE(imprimante_reseau,GENERAL:Vérifiez que l''imprimante est connectée au même réseau WiFi que votre ordinateur et qu''elle est allumée.)','J''ai vérifié la connexion réseau de l''imprimante');")
  (sql-exec "INSERT INTO rules VALUES ('R129','F45','ACTION_RECOMMANDEE(vpn_hs,GENERAL:Vérifiez vos identifiants de connexion VPN et assurez-vous que la configuration est correcte.)','J''ai vérifié mes identifiants et la configuration VPN');")
  (sql-exec "INSERT INTO rules VALUES ('R130','F46','ACTION_RECOMMANDEE(ip_invalide,GENERAL:Renouvelez votre adresse IP : ouvrez une invite de commandes et tapez ipconfig /release puis ipconfig /renew.)','J''ai renouvelé mon adresse IP');")
  (sql-exec "INSERT INTO rules VALUES ('R131','F47','ACTION_RECOMMANDEE(arret_automatique,GENERAL:Vérifiez la température de votre processeur et carte graphique avec un logiciel comme HWMonitor ou Core Temp.)','J''ai vérifié les températures avec HWMonitor ou Core Temp');")
  (sql-exec "INSERT INTO rules VALUES ('R132','F48','ACTION_RECOMMANDEE(ventilateur_arret,GENERAL:Vérifiez que le câble d''alimentation du ventilateur est correctement branché sur la carte mère.)','J''ai vérifié le branchement du câble du ventilateur');")
  (sql-exec "INSERT INTO rules VALUES ('R133','F49','ACTION_RECOMMANDEE(chaleur_excessive,GENERAL:Utilisez votre ordinateur dans une pièce fraîche et bien ventilée. Évitez de le poser sur des surfaces molles.)','J''ai amélioré la ventilation de mon espace de travail');")
  (sql-exec "INSERT INTO rules VALUES ('R134','F50','ACTION_RECOMMANDEE(performances_reduites,GENERAL:Vérifiez les paramètres de gestion de l''énergie et passez en mode Performances élevées si nécessaire.)','J''ai changé le mode d''alimentation en Performances élevées');")
  (sql-exec "INSERT INTO rules VALUES ('R135','F51','ACTION_RECOMMANDEE(bruit_irregulier,GENERAL:Nettoyez le ventilateur avec de l''air comprimé. Si le bruit persiste, le ventilateur doit probablement être remplacé.)','J''ai nettoyé le ventilateur avec de l''air comprimé');")
  (sql-exec "INSERT INTO rules VALUES ('R136','F52','ACTION_RECOMMANDEE(popups,GENERAL:Vérifiez les programmes au démarrage et désinstallez les logiciels suspects récemment installés.)','J''ai vérifié et désinstallé les programmes suspects');")
  (sql-exec "INSERT INTO rules VALUES ('R137','F53','ACTION_RECOMMANDEE(redirections,GENERAL:Lancez une analyse complète avec votre antivirus et vérifiez les extensions de votre navigateur.)','J''ai lancé une analyse antivirus et vérifié les extensions');")
  (sql-exec "INSERT INTO rules VALUES ('R138','F54','ACTION_RECOMMANDEE(fichiers_cryptes,GENERAL:IMPORTANT : Ne payez jamais la rançon ! Consultez nomoreransom.org pour des outils de déchiffrement gratuits.)','J''ai consulté nomoreransom.org pour chercher un outil de déchiffrement');")
  (sql-exec "INSERT INTO rules VALUES ('R139','F55','ACTION_RECOMMANDEE(mot_passe_change,GENERAL:Changez immédiatement tous vos mots de passe depuis un autre appareil sécurisé.)','J''ai changé tous mes mots de passe depuis un autre appareil');")
  (sql-exec "INSERT INTO rules VALUES ('R140','F56','ACTION_RECOMMANDEE(logiciels_inconnus,GENERAL:Allez dans Paramètres > Applications et désinstallez tous les programmes que vous n''avez pas installés vous-même.)','J''ai désinstallé les programmes inconnus');")
  (sql-exec "INSERT INTO rules VALUES ('R141','F57','ACTION_RECOMMANDEE(parefeu_desactive,GENERAL:Activez le pare-feu Windows dans Sécurité Windows > Pare-feu et protection réseau.)','J''ai activé le pare-feu Windows');")
  (sql-exec "INSERT INTO rules VALUES ('R142','F58','ACTION_RECOMMANDEE(mises_jour_manquees,GENERAL:Installez toutes les mises à jour disponibles via Paramètres > Windows Update > Rechercher des mises à jour.)','J''ai installé les mises à jour Windows');")
  (sql-exec "INSERT INTO rules VALUES ('R143','F59','ACTION_RECOMMANDEE(bruit_disque,GENERAL:Surveillez la température et l''état du disque avec CrystalDiskInfo. Des bruits anormaux peuvent indiquer une panne imminente.)','J''ai surveillé l''état du disque avec CrystalDiskInfo');")
  (sql-exec "INSERT INTO rules VALUES ('R144','F60','ACTION_RECOMMANDEE(espace_plein,GENERAL:Utilisez l''outil Nettoyage de disque Windows : tapez cleanmgr dans la recherche et suivez les instructions.)','J''ai utilisé le Nettoyage de disque Windows');")
  (sql-exec "INSERT INTO rules VALUES ('R145','F61','ACTION_RECOMMANDEE(erreurs_lecture,GENERAL:Exécutez l''outil de vérification du disque : clic droit sur le disque > Propriétés > Outils > Vérifier.)','J''ai exécuté l''outil de vérification du disque');")
  (sql-exec "INSERT INTO rules VALUES ('R146','F62','ACTION_RECOMMANDEE(disque_detecte,GENERAL:Vérifiez dans le BIOS que le disque est bien détecté. Contrôlez les câbles SATA et d''alimentation.)','J''ai vérifié la détection du disque dans le BIOS');")
  (sql-exec "INSERT INTO rules VALUES ('R147','F63','ACTION_RECOMMANDEE(partition_corrompue,GENERAL:Utilisez l''outil gratuit TestDisk pour tenter de réparer la partition corrompue.)','J''ai utilisé TestDisk pour réparer la partition');")
  (sql-exec "INSERT INTO rules VALUES ('R148','F64','ACTION_RECOMMANDEE(fragmentation,GENERAL:Défragmentez votre disque dur via l''outil Optimiser les lecteurs de Windows. Note : ne pas défragmenter un SSD.)','J''ai défragmenté mon disque dur');")
  (sql-exec "INSERT INTO rules VALUES ('R149','F65','ACTION_RECOMMANDEE(ssd_hdd,GENERAL:Vérifiez les paramètres SATA dans le BIOS et assurez-vous que le mode AHCI est activé pour les SSD.)','J''ai vérifié et activé le mode AHCI dans le BIOS');")
  (sql-exec "INSERT INTO rules VALUES ('R150','F66','ACTION_RECOMMANDEE(clavier_hs,GENERAL:Testez avec un autre clavier USB pour déterminer si le problème vient du clavier ou de l''ordinateur.)','J''ai testé avec un autre clavier USB');")
  (sql-exec "INSERT INTO rules VALUES ('R151','F67','ACTION_RECOMMANDEE(souris_hs,GENERAL:Testez avec une autre souris USB pour déterminer si le problème vient de la souris ou de l''ordinateur.)','J''ai testé avec une autre souris USB');")
  (sql-exec "INSERT INTO rules VALUES ('R152','F68','ACTION_RECOMMANDEE(usb_hs,GENERAL:Testez différents ports USB, notamment ceux à l''arrière de l''ordinateur qui sont souvent plus fiables.)','J''ai testé différents ports USB');")
  (sql-exec "INSERT INTO rules VALUES ('R153','F69','ACTION_RECOMMANDEE(carte_son_hs,GENERAL:Mettez à jour les pilotes audio depuis le Gestionnaire de périphériques ou le site du fabricant.)','J''ai mis à jour les pilotes audio');")
  (sql-exec "INSERT INTO rules VALUES ('R154','F70','ACTION_RECOMMANDEE(batterie_hs,GENERAL:Vérifiez le connecteur de la batterie. Si le problème persiste, la batterie doit probablement être remplacée.)','J''ai vérifié le connecteur de la batterie');")
  (sql-exec "INSERT INTO rules VALUES ('R155','F71','ACTION_RECOMMANDEE(webcam_hs,GENERAL:Mettez à jour les pilotes de la webcam et vérifiez les paramètres de confidentialité dans Windows.)','J''ai mis à jour les pilotes de la webcam');")
  (sql-exec "INSERT INTO rules VALUES ('R156','F72','ACTION_RECOMMANDEE(windows_corrompu,GENERAL:Exécutez la commande sfc /scannow dans une invite de commandes administrateur pour réparer les fichiers système.)','J''ai exécuté sfc /scannow');")
  (sql-exec "INSERT INTO rules VALUES ('R157','F73','ACTION_RECOMMANDEE(pilotes_obsoletes,GENERAL:Mettez à jour vos pilotes via Windows Update ou téléchargez-les depuis le site du fabricant de votre ordinateur.)','J''ai mis à jour mes pilotes');")
  (sql-exec "INSERT INTO rules VALUES ('R158','F74','ACTION_RECOMMANDEE(services_arretes,GENERAL:Vérifiez les services Windows : tapez services.msc dans la recherche et vérifiez que les services essentiels sont démarrés.)','J''ai vérifié les services Windows');")
  (sql-exec "INSERT INTO rules VALUES ('R159','F75','ACTION_RECOMMANDEE(registre_corrompu,GENERAL:Exécutez DISM /Online /Cleanup-Image /RestoreHealth puis sfc /scannow dans une invite de commandes administrateur.)','J''ai exécuté DISM et sfc /scannow');")
  (sql-exec "INSERT INTO rules VALUES ('R160','F76','ACTION_RECOMMANDEE(fichiers_manquants,GENERAL:Restaurez les fichiers système avec la commande sfc /scannow dans une invite de commandes administrateur.)','J''ai restauré les fichiers système avec sfc /scannow');")
  (sql-exec "INSERT INTO rules VALUES ('R161','F77','ACTION_RECOMMANDEE(profil_corrompu,GENERAL:Créez un nouveau compte utilisateur Windows et transférez vos fichiers depuis l''ancien profil.)','J''ai créé un nouveau compte utilisateur');")
  (sql-exec "INSERT INTO rules VALUES ('R162','F78','ACTION_RECOMMANDEE(logiciel_plante,GENERAL:Vérifiez la compatibilité du logiciel avec votre version de Windows et mettez-le à jour si possible.)','J''ai vérifié la compatibilité et mis à jour le logiciel');")
  (sql-exec "INSERT INTO rules VALUES ('R163','F79','ACTION_RECOMMANDEE(installation_echoue,GENERAL:Vérifiez que vous disposez de suffisamment d''espace disque pour l''installation du logiciel.)','J''ai vérifié l''espace disque disponible');")
  (sql-exec "INSERT INTO rules VALUES ('R167','F80','ACTION_RECOMMANDEE(logiciel_demarre,GENERAL:Vérifiez l''intégrité des fichiers du logiciel. Désinstallez-le et réinstallez-le si nécessaire.)','J''ai réinstallé le logiciel');")
  (sql-exec "INSERT INTO rules VALUES ('R164','F81','ACTION_RECOMMANDEE(incompatibilite,GENERAL:Faites clic droit sur le programme > Propriétés > Compatibilité et activez le mode de compatibilité.)','J''ai activé le mode de compatibilité');")
  (sql-exec "INSERT INTO rules VALUES ('R165','F82','ACTION_RECOMMANDEE(droits_admin,GENERAL:Faites clic droit sur le programme et choisissez Exécuter en tant qu''administrateur.)','J''ai exécuté le programme en tant qu''administrateur');")
  (sql-exec "INSERT INTO rules VALUES ('R166','F83','ACTION_RECOMMANDEE(dependances,GENERAL:Installez les composants requis : Visual C++ Redistributable, .NET Framework ou DirectX selon l''erreur affichée.)','J''ai installé les composants requis');")
  (sql-exec "INSERT INTO rules VALUES ('R172','F84','ACTION_RECOMMANDEE(conflit_logiciels,GENERAL:Identifiez et désinstallez le logiciel conflictuel. Essayez de démarrer en mode sans échec pour tester.)','J''ai identifié et désinstallé le logiciel conflictuel');")
  (sql-exec "INSERT INTO rules VALUES ('R168','F85','ACTION_RECOMMANDEE(imprimante_hs,GENERAL:Vérifiez les connexions de l''imprimante et mettez à jour ses pilotes depuis le site du fabricant.)','J''ai vérifié les connexions et mis à jour les pilotes de l''imprimante');")
  (sql-exec "INSERT INTO rules VALUES ('R169','F86','ACTION_RECOMMANDEE(scanner_hs,GENERAL:Mettez à jour les pilotes du scanner depuis le site du fabricant ou via Windows Update.)','J''ai mis à jour les pilotes du scanner');")
  (sql-exec "INSERT INTO rules VALUES ('R170','F87','ACTION_RECOMMANDEE(cle_usb_hs,GENERAL:Essayez la clé USB sur un autre port, de préférence à l''arrière de l''ordinateur.)','J''ai testé la clé USB sur un autre port');")
  (sql-exec "INSERT INTO rules VALUES ('R171','F88','ACTION_RECOMMANDEE(disque_externe_hs,GENERAL:Vérifiez le câble USB et l''alimentation du disque externe. Essayez un autre câble si possible.)','J''ai vérifié le câble USB et l''alimentation du disque externe');")
  (sql-exec "INSERT INTO rules VALUES ('R184','F89','ACTION_RECOMMANDEE(casque_mulet,GENERAL:Vérifiez que le casque est bien branché et sélectionné comme périphérique de sortie audio par défaut.)','J''ai vérifié le branchement et la sélection du casque');")
  (sql-exec "INSERT INTO rules VALUES ('R173','F90','ACTION_RECOMMANDEE(microphone_mulet,GENERAL:Ajustez les niveaux du microphone dans Paramètres > Son > Entrée et vérifiez qu''il n''est pas coupé.)','J''ai ajusté les niveaux du microphone');")
  (sql-exec "INSERT INTO rules VALUES ('R174','F91','ACTION_RECOMMANDEE(aucun_son,GENERAL:Vérifiez le périphérique audio par défaut : clic droit sur l''icône son > Paramètres du son.)','J''ai vérifié le périphérique audio par défaut');")
  (sql-exec "INSERT INTO rules VALUES ('R175','F92','ACTION_RECOMMANDEE(son_deforme,GENERAL:Désactivez les améliorations audio : clic droit sur l''icône son > Paramètres > Propriétés du périphérique > Améliorations.)','J''ai désactivé les améliorations audio');")
  (sql-exec "INSERT INTO rules VALUES ('R176','F93','ACTION_RECOMMANDEE(volume_faible,GENERAL:Ajustez les niveaux audio dans Paramètres > Son et vérifiez le volume dans le mélangeur de volume.)','J''ai ajusté les niveaux audio');")
  (sql-exec "INSERT INTO rules VALUES ('R177','F94','ACTION_RECOMMANDEE(son_mono,GENERAL:Vérifiez la connexion de vos enceintes ou casque. Le câble jack pourrait être mal branché ou endommagé.)','J''ai vérifié la connexion jack de mes enceintes/casque');")
  (sql-exec "INSERT INTO rules VALUES ('R178','F95','ACTION_RECOMMANDEE(echo_micro,GENERAL:Désactivez la réduction de bruit et l''écho dans les paramètres audio de votre application de communication.)','J''ai désactivé la réduction de bruit et l''écho');")
  (sql-exec "INSERT INTO rules VALUES ('R179','F96','ACTION_RECOMMANDEE(resolution,GENERAL:Ajustez la résolution dans Paramètres > Affichage > Résolution d''écran. Choisissez la résolution recommandée.)','J''ai ajusté la résolution d''écran');")
  (sql-exec "INSERT INTO rules VALUES ('R180','F97','ACTION_RECOMMANDEE(ecran_flou,GENERAL:Ajustez la netteté de votre moniteur via ses boutons physiques ou vérifiez que la résolution correspond à celle native de l''écran.)','J''ai ajusté la netteté et vérifié la résolution native');")
  (sql-exec "INSERT INTO rules VALUES ('R183','F98','ACTION_RECOMMANDEE(couleurs_deformees,GENERAL:Calibrez les couleurs de votre écran : Paramètres > Affichage > Calibrer les couleurs de l''écran.)','J''ai calibré les couleurs de mon écran');")
  (sql-exec "INSERT INTO rules VALUES ('R181','F99','ACTION_RECOMMANDEE(ecran_clignote,GENERAL:Vérifiez la fréquence de rafraîchissement dans Paramètres > Affichage > Paramètres d''écran avancés.)','J''ai vérifié la fréquence de rafraîchissement');")
  (sql-exec "INSERT INTO rules VALUES ('R182','F100','ACTION_RECOMMANDEE(ecrans_multiples,GENERAL:Allez dans Paramètres > Affichage et cliquez sur Détecter pour rechercher les écrans connectés.)','J''ai utilisé la fonction Détecter pour mes écrans');")

  ;; NOUVELLES RÈGLES COMPLEXES SUPPLÉMENTAIRES

  ;; Logiciels (3-4 faits)
  (sql-exec "INSERT INTO rules VALUES ('R228','F78;F81;F83','CAUSE_POSSIBLE(logiciel,incompatibilite_complete)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R229','CAUSE_POSSIBLE(logiciel,incompatibilite_complete)','ACTION_RECOMMANDEE(logiciel,PRECIS:INCOMPATIBILITÉ CONFIRMÉE ! Logiciel incompatible + dépendances manquantes. Désinstallez-le, installez les composants requis (Visual C++, .NET Framework), puis réinstallez en mode compatibilité.)','J''ai réinstallé le logiciel avec les composants requis en mode compatibilité');")

  (sql-exec "INSERT INTO rules VALUES ('R230','F78;F84;F30','CAUSE_POSSIBLE(logiciel,conflit_ressources)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R231','CAUSE_POSSIBLE(logiciel,conflit_ressources)','ACTION_RECOMMANDEE(logiciel,PRECIS:CONFLIT DE RESSOURCES ! Logiciel qui plante + conflit + RAM saturée. Fermez les autres applications, augmentez la RAM virtuelle, ou désinstallez le logiciel conflictuel.)','J''ai fermé les applications et résolu le conflit de ressources');")

  ;; Matériel (3-4 faits)
  (sql-exec "INSERT INTO rules VALUES ('R232','F66;F67;F68','CAUSE_POSSIBLE(materiel,probleme_usb_general)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R233','CAUSE_POSSIBLE(materiel,probleme_usb_general)','ACTION_RECOMMANDEE(materiel,PRECIS:PROBLÈME USB GLOBAL ! Clavier + souris + ports USB défaillants. Vérifiez les pilotes USB dans le Gestionnaire de périphériques ou testez avec un hub USB alimenté.)','J''ai vérifié les pilotes USB et/ou testé avec un hub USB alimenté');")

  (sql-exec "INSERT INTO rules VALUES ('R234','F70;F2;F47','CAUSE_POSSIBLE(materiel,probleme_alimentation_portable)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R235','CAUSE_POSSIBLE(materiel,probleme_alimentation_portable)','ACTION_RECOMMANDEE(materiel,PRECIS:PROBLÈME ALIMENTATION ! Batterie HS + PC ne s''allume pas + arrêts = problème d''alimentation. Testez avec le chargeur seul (sans batterie) pour isoler le problème.)','J''ai testé avec le chargeur seul sans la batterie');")

  ;; Audio (3 faits)
  (sql-exec "INSERT INTO rules VALUES ('R236','F91;F89;F69','CAUSE_POSSIBLE(audio,probleme_audio_complet)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R237','CAUSE_POSSIBLE(audio,probleme_audio_complet)','ACTION_RECOMMANDEE(audio,PRECIS:PROBLÈME AUDIO COMPLET ! Aucun son + casque muet + carte son non détectée. Réinstallez les pilotes audio ou vérifiez si la carte son est défaillante.)','J''ai réinstallé les pilotes audio');")

  (sql-exec "INSERT INTO rules VALUES ('R238','F92;F93;F73','CAUSE_POSSIBLE(audio,pilotes_audio_defaillants)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R239','CAUSE_POSSIBLE(audio,pilotes_audio_defaillants)','ACTION_RECOMMANDEE(audio,PRECIS:PILOTES AUDIO DÉFAILLANTS ! Son déformé + volume faible + pilotes obsolètes. Désinstallez complètement les pilotes audio et réinstallez la dernière version depuis le site du fabricant.)','J''ai désinstallé et réinstallé les pilotes audio');")

  ;; Affichage (3-4 faits)
  (sql-exec "INSERT INTO rules VALUES ('R240','F96;F97;F99;F73','CAUSE_POSSIBLE(affichage,probleme_graphique_complet)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R241','CAUSE_POSSIBLE(affichage,probleme_graphique_complet)','ACTION_RECOMMANDEE(affichage,PRECIS:PROBLÈME GRAPHIQUE MAJEUR ! Résolution + flou + scintillement + pilotes obsolètes. Désinstallez les pilotes graphiques avec DDU (Display Driver Uninstaller) puis réinstallez la dernière version.)','J''ai réinstallé les pilotes graphiques avec DDU');")

  (sql-exec "INSERT INTO rules VALUES ('R242','F3;F100;F16','CAUSE_POSSIBLE(affichage,carte_graphique_defaillante)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R243','CAUSE_POSSIBLE(affichage,carte_graphique_defaillante)','ACTION_RECOMMANDEE(affichage,PRECIS:CARTE GRAPHIQUE SUSPECTE ! Écran noir + écrans non détectés + carte dédiée installée. Testez sans la carte graphique dédiée pour vérifier si elle est défaillante.)','J''ai testé sans la carte graphique dédiée');")

  ;; Périphériques (3 faits)
  (sql-exec "INSERT INTO rules VALUES ('R244','F85;F86;F44','CAUSE_POSSIBLE(peripheriques,probleme_reseau_impression)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R245','CAUSE_POSSIBLE(peripheriques,probleme_reseau_impression)','ACTION_RECOMMANDEE(peripheriques,PRECIS:PROBLÈME RÉSEAU IMPRESSION ! Imprimante + scanner + réseau inaccessibles. Vérifiez la connexion réseau de vos périphériques et réinstallez les pilotes.)','J''ai vérifié la connexion réseau et réinstallé les pilotes');")

  (sql-exec "INSERT INTO rules VALUES ('R246','F87;F88;F68','CAUSE_POSSIBLE(peripheriques,probleme_stockage_externe)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R247','CAUSE_POSSIBLE(peripheriques,probleme_stockage_externe)','ACTION_RECOMMANDEE(peripheriques,PRECIS:PROBLÈME STOCKAGE EXTERNE ! Clé USB + disque externe + ports USB défaillants. Testez vos périphériques sur un autre ordinateur pour isoler le problème.)','J''ai testé mes périphériques de stockage sur un autre ordinateur');")

  ;; Cross-catégories complexes (4+ faits)
  (sql-exec "INSERT INTO rules VALUES ('R248','F1;F27;F11;F50;F7','CAUSE_POSSIBLE(global,systeme_en_souffrance)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R249','CAUSE_POSSIBLE(global,systeme_en_souffrance)','ACTION_RECOMMANDEE(global,PRECIS:SYSTÈME EN GRANDE DIFFICULTÉ ! Lenteurs + gels + surchauffe + throttling + CPU saturé. Votre ordinateur a besoin d''une maintenance complète : nettoyage thermique, ajout de RAM, et passage au SSD.)','J''ai effectué une maintenance complète (nettoyage, RAM, SSD)');")

  (sql-exec "INSERT INTO rules VALUES ('R250','F9;F1;F52;F41;F57','CAUSE_POSSIBLE(global,infection_impactant_tout)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R251','CAUSE_POSSIBLE(global,infection_impactant_tout)','ACTION_RECOMMANDEE(global,PRECIS:INFECTION GÉNÉRALISÉE ! Malware + lenteurs + pop-ups + internet lent + pare-feu désactivé. Démarrez en mode sans échec, lancez plusieurs analyses (Malwarebytes, AdwCleaner), puis réactivez le pare-feu.)','J''ai nettoyé l''infection en mode sans échec et réactivé le pare-feu');")

  (sql-exec "INSERT INTO rules VALUES ('R252','F32;F1;F10;F60;F73','CAUSE_POSSIBLE(global,maintenance_complete_necessaire)',NULL);")
  (sql-exec "INSERT INTO rules VALUES ('R253','CAUSE_POSSIBLE(global,maintenance_complete_necessaire)','ACTION_RECOMMANDEE(global,PRECIS:MAINTENANCE URGENTE REQUISE ! Démarrage lent + PC lent + disque défaillant + espace plein + pilotes obsolètes. Plan d''action : 1) Libérez de l''espace, 2) Mettez à jour les pilotes, 3) Envisagez un SSD.)','J''ai effectué la maintenance complète (espace, pilotes, SSD)');")
)
