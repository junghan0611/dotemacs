;;; profiles.el --- Doom Emacs Profile Configuration -*- lexical-binding: t; -*-
;;
;; 둠이맥스 프로파일 설정 - 하나의 둠이맥스 설치로 여러 설정 관리
;; 
;; 사용법:
;;   1. doom sync 실행 (프로파일 캐시 생성)
;;   2. emacs --profile starter (특정 프로파일로 실행)
;;   3. doom sync --profile starter (특정 프로파일 동기화)
;;
;; Author: Junghan Kim
;; Created: 2025-09-03
;; Updated: 2025-09-03 - 프로파일 단순화 (3개)

;;; 프로파일 정의
((default  ; Full GUI 버전 (데스크톱)
  ("DOOMDIR" . "~/dotemacs/dotdoomemacs"))

 (starter  ; 통합 경량 버전
  ;; 데스크톱 터미널, Android Native GUI, Termux 모두 지원
  ;; 환경 감지하여 자동 최적화 (Android시 Termux PATH 연동)
  ("DOOMDIR" . "~/repos/gh/dotdoom-starter"))

 (minimal  ; 최소 설정 (테스트/디버깅용)
  ("DOOMDIR" . "~/repos/gh/dotdoom-minimal")))

;;; Notes:
;; - default: 현재 GUI 중심의 풀 기능 설정
;; - starter: 터미널 환경, 원격 서버용 경량 설정
;; - minimal: 문제 해결 및 테스트용 최소 설정
;; - android: Termux 환경용 (계획 중)
;;
;; 각 프로파일은 독립적인 패키지 디렉토리를 가짐:
;; ~/.local/share/doom/[profile-name]/@/
;;
;; 프로파일 간 패키지는 현재 공유되지 않음 (Doom v3에서 개선 예정)

;;; profiles.el ends here