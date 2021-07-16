package main

import (
	"bytes"
	"io"
	"log"
	"os"
	"net/http"
	"net/http/httputil"
	"net/url"
)

func main() {
	stravaAPIURL, err := url.Parse("https://www.strava.com/api/v3")
	if err != nil {
		log.Fatal(err)
	}
	proxy := httputil.NewSingleHostReverseProxy(stravaAPIURL)
	mux := http.NewServeMux()
	mux.HandleFunc("/oauth/token", func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			w.WriteHeader(http.StatusMethodNotAllowed)
			return
		}
		if err := r.ParseForm(); err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		r.Host = stravaAPIURL.Host
		r.PostForm.Add("client_secret", os.Args[0])
		content := r.PostForm.Encode()
		r.Body = io.NopCloser(bytes.NewBufferString(content))
		r.ContentLength = int64(len(content))
		proxy.ServeHTTP(w, r)
	})
	log.Print((&http.Server{
		Addr:    ":9999",
		Handler: mux,
	}).ListenAndServe())
}
